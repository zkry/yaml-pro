;;; yaml-pro-edit.el --- Functions for editing functionality -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.4
;; Homepage: https://github.com/zkry/yaml-pro
;; Keywords: tools

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x yaml-pro-edit-scalar

;;; Code:

(defconst yaml-pro-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      ;;(suppress-keymap map)
      (define-key map (kbd "C-c C-k") #'yaml-pro-edit-quit)
      (define-key map (kbd "C-c C-c") #'yaml-pro-edit-complete)
      (define-key map (kbd "C-c C-s") #'yaml-pro-edit-change-output)))
  "Eeymap of yaml-edit-mode.")

(defconst yaml-pro-edit-buffer-name "*yaml-pro-edit*")
(defvar-local yaml-pro-edit-scalar nil
  "The current scalar being edited on.
Used to fetch location properties on completion.")
(defvar-local yaml-pro-edit-scalar-overlay nil
  "Overlay to go on top of the text currently being edited.")
(defvar-local yaml-pro-edit-parent-buffer nil
  "Reference to the buffer where the edited YAML originated.")
(defvar-local yaml-pro-edit-output-type nil
  "When completing an edit buffer, output scalar according to this.")
(defvar-local yaml-pro-edit-initialization-cache nil
  "Hashmap of YAML path to initialization function.")

(define-minor-mode yaml-pro-edit-mode
  "Minor-mode for editing a YAML scalar in a separate buffer."
  :lighter " YAML-pro"
  :keymap yaml-pro-edit-mode-map)

(defun yaml-pro-edit-cleanup-parent ()
  "Remove overlay and properties of edited text."
  (with-current-buffer (or yaml-pro-edit-parent-buffer (current-buffer))
    (when yaml-pro-edit-scalar-overlay
      (delete-overlay yaml-pro-edit-scalar-overlay)
      (setq yaml-pro-edit-scalar-overlay nil))))

(defun yaml-pro-edit--infer-scalar-indent (scalar-block-string)
  "Infer the scalar indentation SCALAR-BLOCK-STRING."
  (with-temp-buffer
    (insert scalar-block-string)
    (goto-char (point-min))
    (save-excursion
      (let ((min-indent 10000))
        (while (not (eobp))
          (when (not (looking-at " *\n"))
            (let ((ct 0))
              (while (looking-at-p " ")
                (forward-char 1)
                (cl-incf ct))
              (when (< ct min-indent)
                (setq min-indent ct))))
          (forward-line 1))
        min-indent))))

(defun yaml-pro-edit--infer-indent (&optional pos)
  "Infer the YAML indentation of POS or the current point."
  (let ((pos (or pos (point))))
    (goto-char pos)
    (forward-line 0)
    (skip-chars-forward "- \n")
    (current-column)))

(defconst yaml-pro-edit-output-types
  '(("|" . literal)
    ("|-" . literal-strip)
    ("|+" . literal-keep)
    (">" . folded)
    (">-" . folded-strip)
    (">+" . folded-keep)
    ("'"  . single)
    ("\"" . double))
  "Alist of selection item to type key.")

(defun yaml-pro-edit--block-style-annotation (v)
  "Return the completion annotation of the selection string V."
  (let ((annotations
         '(("|" . "keep newlines, single newline at end")
           ("|-" . "keep newlines, strip newlines at end")
           ("|+" . "keep newlines, keep newlines at end")
           (">" . "fold newlines to space, single newline at end")
           (">-" . "fold newlines to space, strip newlines at end")
           (">+" . "fold newlines to space, keep newlines at end")
           ("'" . "nothing escaped")
           ("\"" . "newlines escaped"))))
    (concat " " (cdr (assoc v annotations)))))

(defun yaml-pro-edit--block-output (type)
  "Return the block prefix of TYPE if one exists."
  (when (not (memq type '(single double)))
    (car (rassoc type yaml-pro-edit-output-types))))

(defun yaml-pro-edit-apply-indentation (content indent &optional type)
  "Apply an indentation level of INDENT to the string CONTENT and return."
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (let ((indent-str (make-string indent ?\s)))
      (when (eql type 'single)
        (forward-line 1))
      (while (not (eobp))
        (when (not (looking-at-p "^ *\n"))
          (insert indent-str))
        (forward-line 1)))
    (buffer-string)))

(defun yaml-pro-edit--header-line ()
  "Return the string to display in the header line."
  (let* ((base-text (substitute-command-keys "Edit, then exit with \
`\\[yaml-pro-edit-complete]' or abort with `\\[yaml-pro-edit-quit]'")))
    (if yaml-pro-edit-output-type
        (let* ((type-str
                (or (yaml-pro-edit--block-output yaml-pro-edit-output-type)
                    (and (eql yaml-pro-edit-output-type 'double) "\"\"")
                    (and (eql yaml-pro-edit-output-type 'single) "''")))
               (type-str-prp (propertize type-str 'face 'font-lock-string-face)))
          (concat base-text
                  ". Outputting "
                  type-str-prp
                  (substitute-command-keys
                   ", `\\[yaml-pro-edit-change-output]' to change")))
      (concat base-text
              (substitute-command-keys
               ". Change output with `\\[yaml-pro-edit-change-output]'")))))

(defun yaml-pro-edit-initialize-buffer
    (parent-buffer buffer initial-text type initialize path)
  "Initialize the YAML edit buffer.

PARENT-BUFFER is the buffer from which the YAML was copied.
BUFFER is the editing buffer.  The buffer is initialized with the
text INITIAL-TEXT.

TYPE indicates the type of scalar that the block was copied
from (ex literal or folded).

INITIALIZE, if non-nil, will run before the buffer is
initialized.  The INITIALIZE function will be cached under PATH,
resulting in the function being ran upon subsequent edits."
  (when (not yaml-pro-edit-initialization-cache)
    (setq-local yaml-pro-edit-initialization-cache
                (make-hash-table :test 'equal)))
  (let* ((init-cache yaml-pro-edit-initialization-cache)
         (init-fn (or initialize (and path (gethash path init-cache)))))
    (with-current-buffer buffer
      (when (functionp init-fn)
        (condition-case e
            (progn
              (funcall init-fn)
              (when (and initialize path)
                (puthash path initialize init-cache)))
          (error (message "Initialization failed with: %S"
                          (error-message-string e)))))
      (unless yaml-pro-edit-mode
        (yaml-pro-edit-mode))
      (erase-buffer)
      (setq-local require-final-newline nil)
      (setq-local yaml-pro-edit-parent-buffer parent-buffer)
      (setq-local yaml-pro-edit-output-type nil)
      (when type (setq-local yaml-pro-edit-output-type type))
      (setq header-line-format (yaml-pro-edit--header-line))
      (insert initial-text))))

(defun yaml-pro-edit--extract-scalar-text (scalar-block-string yaml-indentation)
  "Strip SCALAR-BLOCK-STRING of indentation with a basis of YAML-INDENTATION."
  ;; TODO: consider consolidating this logic with that of yaml.el
  (save-match-data
    (let ((indent-ct-num
           (progn (string-match "\\`[^\n]*\\([0-9]+\\) *\n" scalar-block-string)
                  (let ((num-str (match-string 1 scalar-block-string)))
                    (and num-str (+ (string-to-number num-str)
                                    yaml-indentation))))))
      (setq scalar-block-string (string-trim-left scalar-block-string ".*\n"))
      (with-temp-buffer
        (insert scalar-block-string)
        (goto-char (point-min))
        (let* ((indentation
                (or indent-ct-num
                    (yaml-pro-edit--infer-scalar-indent scalar-block-string)))
               (indentation-regexp
                (regexp-quote (make-string indentation ?\s))))
          (while (not (eobp))
            (when (looking-at-p indentation-regexp)
              (delete-char indentation))
            (forward-line 1)))
        (buffer-string)))))

;; Interactive Functions

(defun yaml-pro-edit-change-output ()
  "Change the selected output type after completing the YAML."
  (interactive)
  (unless yaml-pro-edit-mode
    (user-error "not in yaml-pro edit buffer"))
  (let* ((completion-extra-properties
          '(:annotation-function yaml-pro-edit--block-style-annotation))
         (output-type (completing-read "Output: " yaml-pro-edit-output-types))
         (key (cdr (assoc output-type yaml-pro-edit-output-types 'equal))))
    (setq yaml-pro-edit-output-type key)
    (setq header-line-format (yaml-pro-edit--header-line))))

(defun yaml-pro-edit-quit ()
  "Quit the edit buffer."
  (interactive)
  (unless yaml-pro-edit-mode
    (user-error "not in yaml-pro edit buffer"))
  (yaml-pro-edit-cleanup-parent)
  (let ((b (current-buffer)))
    (quit-window)
    (kill-buffer b)))

(defun yaml-pro-edit-complete ()
  "Deleting edit buffer and move its contents to the original YAML buffer."
  (interactive)
  (unless yaml-pro-edit-mode
    (user-error "not in yaml-pro edit buffer"))
  (unless yaml-pro-edit-parent-buffer
    (error "Buffer not connected with yaml buffer"))
  ;; first ensure that buffer ends with at least one newline
  (let* ((edit-buf (current-buffer))
         (edit-str (buffer-substring-no-properties (point-min) (point-max)))
         (type yaml-pro-edit-output-type))
    (when (not (memq type '(single double)))
      (save-excursion
        (goto-char (point-max))
        (unless (looking-back "\n" (- (point) 2))
          (insert "\n")
          (setq edit-str (buffer-substring-no-properties (point-min) (point-max))))))
    (save-excursion
      (with-current-buffer yaml-pro-edit-parent-buffer
        (let* ((pos (get-text-property 0 'yaml-position yaml-pro-edit-scalar))
               (start (car pos))
               (end (cdr pos))
               (indent (yaml-pro-edit--infer-indent start))
               (scalar-indent (yaml-pro-edit--infer-scalar-indent edit-str))
               (indented-edit-str
                (yaml-pro-edit-apply-indentation edit-str (+ indent 2) type))
               (block-header (or (yaml-pro-edit--block-output type)
                                 (and (not type)
                                      (> (length (split-string edit-str "\n")) 1)
                                      ">-")))
               (extract-string (buffer-substring-no-properties start end))
               (end-newline-p (= (aref extract-string
                                       (1- (length extract-string)))
                                 ?\n)))
          (yaml-pro-edit-cleanup-parent)
          (delete-region start end)
          (goto-char start)
          (cond (block-header
                 (if (> scalar-indent 0)
                     (insert block-header "2\n")
                   (insert block-header "\n"))
                 (insert indented-edit-str))
                ((or (eql type 'double)
                     (and (not type)
                          (and (= (length (split-string edit-str "\n")) 1)
                               (string-prefix-p  " " edit-str))))
                 (insert "\"" (replace-regexp-in-string  "\"" "\\\\\""
                                              (replace-regexp-in-string "\n" "\\\\n" edit-str))
                         "\""))
                ((eql type 'single)
                 (insert "'" (string-trim-right (replace-regexp-in-string
                                                 "'" "''"
                                                 indented-edit-str))
                         "'"))
                (t
                 (insert edit-str)))
          ;; following blocks are tricky
          ;; if the text being extracted is a block, then it ends in a newline
          ;;   meaning the resulting code should return a newline
          ;; if the text being extracted is quoted, we need to make sure that
          ;;   the block doesn't add too many lines than needed.
          (when (and (not (memq type '(double single)))
                     (not end-newline-p))
            (delete-char 1))
          (when (and (memq type '(double single))
                     end-newline-p)
            (insert "\n")))))
    (quit-window)
    (kill-buffer edit-buf)))

(declare-function yaml-pro--value-at-point "yaml-pro")
(declare-function yaml-pro--fast-value-at-point "yaml-pro")
(declare-function yaml-pro--path-at-point "yaml-pro")
(declare-function yaml-pro--use-fast-p "yaml-pro")

;;;###autoload
(defun yaml-pro-edit-scalar (p)
  "Edit the scalar value at the point in a separate buffer.
If prefix argument P is provided, prompt user for initialization command."
  (interactive "p")
  (let* ((init-func)
         (fast-p (yaml-pro--use-fast-p))
         (at-scalar (if fast-p (yaml-pro--fast-value-at-point)
                      (yaml-pro--value-at-point)))
         (path (and (not fast-p) (yaml-pro--path-at-point)))
         (parent-buffer (current-buffer)))
    (unless at-scalar
      (user-error "No value found at point"))
    (when (= 4 p)
      (setq init-func (read-command "Initialization command: ")))
    (setq yaml-pro-edit-scalar at-scalar)
    (let* ((bounds (get-text-property 0 'yaml-position at-scalar))
           (start (car bounds))
           (end (cdr bounds))
           (yaml-indentation (yaml-pro-edit--infer-indent start))
           (scalar-text (buffer-substring start end))
           (raw-scalar (yaml-pro-edit--extract-scalar-text
                        scalar-text yaml-indentation))
           (folded-block-p
            (string-match-p "\\` *>\\(?:+\\|-\\)?[0-9]*\n" scalar-text))
           (literal-block-p
            (string-match-p "\\` *|\\(?:+\\|-\\)?[0-9]*\n" scalar-text))
           (strip-p
            (string-match-p "\\` *.-[0-9]*\n" scalar-text))
           (keep-p
            (string-match-p "\\` *.\\+[0-9]*\n" scalar-text))
           (double-quote-string-p
            (string-match-p "\\`\".*\"\\'" scalar-text))
           (single-quote-string-p
            (string-match-p "\\`'\\(.\\|\n\\)*'\\'" scalar-text))
           (type (cond ((and folded-block-p strip-p) 'folded-strip)
                       ((and literal-block-p strip-p) 'literal-strip)
                       ((and folded-block-p keep-p) 'folded-keep)
                       ((and literal-block-p keep-p) 'literal-keep)
                       (folded-block-p 'folded)
                       (literal-block-p 'literal)
                       (single-quote-string-p 'single)
                       (double-quote-string-p 'double))))
      ;; setup overlay and text properties
      (when yaml-pro-edit-scalar-overlay
        (delete-overlay yaml-pro-edit-scalar-overlay))
      (let ((ov (make-overlay start end))
            (read-only
             (list
              (lambda (&rest _)
                (user-error "Can't modify an scalar being edited in a \
dedicated buffer")))))
        (overlay-put ov 'modification-hooks read-only)
        (overlay-put ov 'insert-in-front-hooks read-only)
        (overlay-put ov 'insert-behind-hooks read-only)
        (overlay-put ov 'face 'secondary-selection)
        (setq yaml-pro-edit-scalar-overlay ov))
      (let ((b (get-buffer-create yaml-pro-edit-buffer-name)))
        (if (or folded-block-p literal-block-p)
            ;; we need to pass the text in buffer if type is block
            (yaml-pro-edit-initialize-buffer
             parent-buffer b raw-scalar type init-func path)
          ;; otherwise use its scalar value (to not show quotes)
          (yaml-pro-edit-initialize-buffer
           parent-buffer b at-scalar type init-func path))
        (let ((window (display-buffer b)))
          (when window
            (select-window window)))))))

(provide 'yaml-pro-edit)

;;; yaml-pro-edit.el ends here
