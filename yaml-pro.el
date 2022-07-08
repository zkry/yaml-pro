;;; yaml-pro.el --- Parser-aided YAML editing features -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (yaml "0.4.0"))
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

;; yaml-pro contains a new mode which provides conveniences when
;; editing YAML.  Running `yaml-pro-mode' switches the mode and the
;; following commands are available: `yaml-pro-move-subtree-up',
;; `yaml-pro-next-subtree', `yaml-pro-prev-subtree',
;; `yaml-pro-kill-subtree', `yaml-pro-up-level',
;; `yaml-pro-unfold-at-point', and `yaml-pro-fold-at-point'.

;;; Code:

(require 'yaml)

(defgroup yaml-pro nil
  "YAML editing tools."
  :prefix "yaml-pro-"
  :group 'convenience)

(defface yaml-pro-fold-replacement-face
  '((t :inherit 'font-lock-comment-face))
  "Face for fold replacement text.")

(defvar-local yaml-pro-buffer-tree nil)

(defun yaml-pro--get-buffer-tree ()
  "Return the cached buffer-tree if exists, else regenerate it."
  (or yaml-pro-buffer-tree
      (let ((tree (yaml-parse-tree (buffer-string))))
        (progn
          (setq yaml-pro-buffer-tree tree)
          tree))))

(defun yaml-pro--after-change-hook (_ _ _)
  "Delete cached tree on buffer change."
  (setq yaml-pro-buffer-tree nil))

(defun yaml-pro--get-parent-block* (tree point)
  "Return subtree from TREE that best contain POINT."
  (if (not (listp tree))
      nil
    (let ((sub-blocks (seq-filter #'identity
                                  (seq-map (lambda (st) (yaml-pro--get-parent-block* st point))
                                           tree))))
      (cond
       ((and sub-blocks
             (stringp (car tree))
             (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
                    (start (1+ (car bounds)))
                    (end (1+ (cdr bounds))))
               (and (numberp start) (<= start point end))))
        (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
               (start (1+ (car bounds)))
               (end (1+ (cdr bounds))))
          (throw 'result (list start end))))
       (sub-blocks
        (car sub-blocks))
       ((stringp (car tree))
        (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
               (start (and bounds (1+ (car bounds))))
               (end (and bounds (1+ (cdr bounds)))))
          (if (and (numberp start) (<= start point end))
              (list start end)
            nil)))
       (t nil)))))

(defun yaml-pro-get-parent-block (tree point)
  "Return the nearest parent block in TREE to node in POINT."
  (catch 'result
    (yaml-pro--get-parent-block* tree point)))

(defun yaml-pro-get-block-bounds (tree point)
  "Return subtree from TREE that best contain POINT."
  (if (not (listp tree))
      nil
    (let ((sub-blocks (seq-filter #'identity
                                  (seq-map (lambda (st) (yaml-pro-get-block-bounds st point))
                                           tree))))
      (cond
       (sub-blocks
        ;; TODO should find best match instead of firt (?)
        (car sub-blocks))
       ((and (stringp (car tree)) (not (equal (car tree) "")))
        (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
               (start (and bounds (1+ (car bounds))))
               (end (and bounds (1+ (cdr bounds)))))
          (if (and
               (numberp start)
               (<= start point end)
               (not (= (save-excursion (goto-char start) (beginning-of-line) (point))
                       (save-excursion (goto-char end) (beginning-of-line) (point)))))
              (list start end)
            nil)))
       (t nil)))))

(defun yaml-pro-get-block (tree point)
  "Return subtree from TREE that best contain POINT."
  (if (not (listp tree))
      nil
    (let ((sub-blocks (seq-filter #'identity
                                  (seq-map (lambda (st) (yaml-pro-get-block st point))
                                           tree))))
      (cond
       (sub-blocks
        ;; TODO should find best match instead of firt (?)
        (car sub-blocks))
       ((stringp (car tree))
        (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
               (start (and bounds (1+ (car bounds))))
               (end (and bounds (1+ (cdr bounds)))))
          (if (and (numberp start)
                   (<= start point end)
                   ;; hack to get small maps to not get selected
                   (yaml-pro--fix-bounds (list start end)))
              (list start end)
            nil)))
       (t nil)))))

(defun yaml-pro--fix-bounds (bounds)
  "Adjust BOUNDS to proper fold points."
  (seq-let (beg end) bounds
    (save-excursion
      (goto-char beg)
      (cond
       ((looking-at-p "[ \t]*-")
        (setq beg (1- (point))))
       ((and (not (looking-at "{"))
             (not (looking-at "\\[")))
        (end-of-line)
        (setq beg (point)))
       ((looking-at-p "\\(\\[\\|{\\)")
        (setq beg (1+ (point))))))
    (save-excursion
      (goto-char end)
      (cond
       ((looking-back "\n" (- (point) 2))
        (setq end (1- end)))
       ((looking-back "\\(\\]\\|\\}\\)" (- (point) 2))
        (setq end (1- end)))))
    (if (>= beg end)
        nil
      (list beg end))))

(defun yaml-pro--path-at-point ()
  "Return the object path to current point.

NOTE: This is an experimental feature."
  ;; first look for current position
  (let* ((parse (yaml-parse-string-with-pos (buffer-string)))
         (path (yaml-pro--search-location parse (point) '())))
    (seq-map
     (lambda (s)
       (set-text-properties 0 (length s) nil s)
       s)
     (or (nreverse path)
         ;; if not found, go back before ":" and try again
         (save-excursion
           (when (looking-back "[ \"a-zA-Z_-]+: +[ \"a-zA-Z_-]+" (- (point) 60))
             (beginning-of-line)
             (skip-chars-forward " ")
             (let ((path (yaml-pro--search-location parse (point) '())))
               (nreverse path))))))))

(defun yaml-pro--find-node (parse point)
  (catch 'done
   (cond
    ((listp parse)
     (dolist (item parse)
       (let ((res (yaml-pro--find-node (cdr item) point)))
         (when res
           (throw 'done res)))))
    ((stringp parse)
     (let* ((bounds (get-text-property 0 'yaml-position parse))
            (start (and bounds (1+ (car bounds))))
            (end (and bounds (1+ (cdr bounds)))))
       (if (<= start point end)
           parse
         nil))))))

(defun yaml-pro--value-at-point ()
  (let* ((parse (yaml-parse-string-with-pos (buffer-string)))
         (val (yaml-pro--find-node parse (point))))
    val))

(defun yaml-pro-hide-overlay (ov)
  "Put fold-related properties on overlay OV."
  (overlay-put ov 'invisible 'origami)
  (overlay-put ov 'display origami-fold-replacement)
  (overlay-put ov'face 'yaml-pro-fold-replacement-face))

(defun yaml-pro-show-overlay (ov)
  "Remove fold-related properties of overlay OV."
  (overlay-put ov 'invisible nil)
  (overlay-put ov 'display nil)
  (overlay-put ov 'face nil))

(defconst yaml-pro-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      ;;(suppress-keymap map)
      (define-key map (kbd "C-c C-k") #'yaml-pro-edit-quit)
      (define-key map (kbd "C-c C-c") #'yaml-pro-edit-complete)
      (define-key map (kbd "C-c C-s") #'yaml-pro-edit-change-output))))

(defconst yaml-pro-edit-buffer-name "*yaml-pro-edit*")
(defvar-local yaml-pro-edit-scalar nil)
(defvar-local yaml-pro-edit-scalar-overlay nil)
(defvar-local yaml-pro-edit-parent-buffer nil)

(defvar-local yaml-pro-edit-output-type nil)

(define-minor-mode yaml-pro-edit-mode
  ""
  :lighter " YAML-pro"
  :keymap yaml-pro-edit-mode-map)

(defun yaml-pro-edit-cleanup-parent ()
  "Remove overlay and properties of edited text."
  (with-current-buffer (or yaml-pro-edit-parent-buffer (current-buffer))
    (when yaml-pro-edit-scalar-overlay
      (let ((start (overlay-start yaml-pro-edit-scalar-overlay))
            (end (overlay-end yaml-pro-edit-scalar-overlay))
            (inhibit-read-only t))
        (remove-text-properties start end '(read-only t)))
      (delete-overlay yaml-pro-edit-scalar-overlay)
      (setq yaml-pro-edit-scalar-overlay nil))))

(defun yaml-pro-edit--infer-scalar-indent (scalar-block-string)
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
  (let ((pos (or pos (point))))
    (goto-char pos)
    (forward-line 0)
    (skip-chars-forward " \n")
    (current-column)))

(defconst yaml-pro-edit-output-types
  '(("| (keep newlines, single newline at end)" . literal)
    ("|- (keep newlines, strip newlines at end)" . literal-strip)
    ("|+ (keep newlines, keep newlines at end)" . literal-keep)
    ("> (fold newlines to space, single newline at end)" . folded)
    (">- (fold newlines to space, strip newlines at end)" . folded-strip)
    (">+ (fold newlines to space, keep newlines at end)" . folded-keep)
    ("' (nothing escaped)"  . single)
    ("\" (newlines escaped)" . double)))

(defun yaml-pro-edit--block-output (type)
  (cdr (assoc type '((literal . "|")
                     (literal-strip . "|-")
                     (literal-keep . "|+")
                     (folded . ">")
                     (folded-strip . ">-")
                     (folded-keep . ">+")))))

(defun yaml-pro-edit-change-output ()
  ""
  (interactive)
  (unless yaml-pro-edit-mode
    (user-error "not in yaml-pro edit buffer"))
  (let* ((output-type (completing-read "Output: " yaml-pro-edit-output-types))
         (key (cdr (assoc output-type yaml-pro-edit-output-types 'equal))))
    (setq yaml-pro-edit-output-type key)
    (setq header-line-format (yaml-pro-edit--header-line))))

(defun yaml-pro-edit-quit ()
  ""
  (interactive)
  (unless yaml-pro-edit-mode
    (user-error "not in yaml-pro edit buffer"))
  (yaml-pro-edit-cleanup-parent)
  (let ((b (current-buffer)))
    (quit-window)
    (kill-buffer b)))

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

(defun yaml-pro-edit-complete ()
  ""
  (interactive)
  (unless yaml-pro-edit-mode
    (user-error "not in yaml-pro edit buffer"))
  (unless yaml-pro-edit-parent-buffer
    (error "buffer not connected with yaml buffer"))
  (let ((edit-buf (current-buffer))
        (edit-str (buffer-substring-no-properties (point-min) (point-max)))
        (type yaml-pro-edit-output-type))
    (save-excursion
      (with-current-buffer yaml-pro-edit-parent-buffer
        (let* ((pos (get-text-property 0 'yaml-position yaml-pro-edit-scalar))
               (start (car pos))
               (end (cdr pos))
               (indent (yaml-pro-edit--infer-indent start))
               (scalar-indent (yaml-pro-edit--infer-scalar-indent edit-str))
               (indented-edit-str (yaml-pro-edit-apply-indentation edit-str (+ indent 2) type))
               (block-header (or (yaml-pro-edit--block-output type)
                                 (and (not type)
                                      (> (length (string-lines edit-str)) 1)
                                      ">-"))))
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
                          (and (= (length (string-lines edit-str)) 1)
                               (string-prefix-p  " " edit-str))))
                 (insert "\"" (string-replace "\"" "\\\""
                                              (string-replace "\n" "\\n" edit-str))
                         "\""))
                ((eql type 'single)
                 (insert "'" (string-replace "'" "''" indented-edit-str) "'"))
                (t
                 (insert edit-str))))))
    (quit-window)
    (kill-buffer edit-buf)))

(defun yaml-pro-edit--header-line ()
  (let* ((base-text (substitute-command-keys "Edit, then exit with \
`\\[yaml-pro-edit-complete]' or abort with `\\[yaml-pro-edit-quit]'"))
         )
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

(defun yaml-pro-initialize-edit-buffer (parent-buffer buffer initial-text &optional type)
  (with-current-buffer buffer
    (unless yaml-pro-edit-mode
      (yaml-pro-edit-mode))
    (erase-buffer)
    (setq-local require-final-newline nil)
    (setq-local yaml-pro-edit-parent-buffer parent-buffer)
    (setq-local yaml-pro-edit-output-type nil)
    (when type (setq-local yaml-pro-edit-output-type type))
    (setq header-line-format (yaml-pro-edit--header-line))
    (insert initial-text)
    (when (memq type '(folded-strip literal-strip))
      (save-excursion
        (goto-char (point-max))
        (delete-char -1)))))

(defun yaml-pro-edit--extract-scalar-text (scalar-block-string yaml-indent)
  (save-match-data
    (let ((indent-ct-num (progn (string-match "\\`[^\n]*\\([0-9]+\\) *\n" scalar-block-string)
                                (let ((num-str (match-string 1 scalar-block-string)))
                                  (and num-str (+ (string-to-number num-str)
                                                  yaml-indent))))))
      (setq scalar-block-string (string-trim-left scalar-block-string ".*\n"))
      (with-temp-buffer
        (insert scalar-block-string)
        (goto-char (point-min))
        (let* ((indentation (or indent-ct-num
                                (yaml-pro-edit--infer-scalar-indent scalar-block-string)))
               (indentation-regexp
                (regexp-quote (make-string indentation ?\s))))
          (while (not (eobp))
            (when (looking-at-p indentation-regexp)
              (delete-char indentation))
            (forward-line 1)))
        (buffer-string)))))

(defun yaml-pro-edit-scalar ()
  "Edit the scalar value at the point in a separate buffer."
  (interactive)
  (let ((at-scalar (yaml-pro--value-at-point))
        (parent-buffer (current-buffer)))
    (unless at-scalar
      (user-error "no value found at point"))
    (setq yaml-pro-edit-scalar at-scalar)
    (let* ((bounds (get-text-property 0 'yaml-position at-scalar))
           (start (car bounds))
           (end (cdr bounds))
           (yaml-indent (yaml-pro-edit--infer-indent start))
           (scalar-text (buffer-substring start end))
           (raw-scalar (yaml-pro-edit--extract-scalar-text
                        scalar-text yaml-indent))
           (folded-block-p (string-match-p "\\` *>\\(?:+\\|-\\)?[0-9]*\n" scalar-text))
           (literal-block-p (string-match-p "\\` *|\\(?:+\\|-\\)?[0-9]*\n" scalar-text))
           (strip-p (string-match-p "\\` *.-[0-9]*\n" scalar-text))
           (keep-p (string-match-p "\\` *.\\+[0-9]*\n" scalar-text))
           (double-quote-string-p (string-match-p "\\`\".*\"\\'" scalar-text))
           (single-quote-string-p (string-match-p "\\`'.*'\\'" scalar-text))
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
      (let ((ov (make-overlay start end)))
          (overlay-put ov 'face 'secondary-selection)
          (setq yaml-pro-edit-scalar-overlay ov)
          (add-text-properties start end '(read-only t)))
      (let ((b (get-buffer-create yaml-pro-special-buffer-name)))
        (if (or folded-block-p literal-block-p)
            ;; we need to pass the text in buffer if type is block 
            (yaml-pro-initialize-edit-buffer parent-buffer b raw-scalar type)
          ;; otherwise use its scalar value (to not show quotes)
          (yaml-pro-initialize-edit-buffer parent-buffer b at-scalar type))
        (switch-to-buffer-other-window b)))))

(defun yaml-pro-fold-at-point ()
  "Fold YAML at point."
  (interactive)
  (save-excursion
    (skip-syntax-forward " " (line-end-position))
    (let ((parse-tree (yaml-pro--get-buffer-tree)))
      (let* ((bounds (yaml-pro--fix-bounds (yaml-pro-get-block parse-tree (point))))
             (beg (car bounds))
             (end (cadr bounds)))
        (when bounds
          ;; Delete all inner folds before creating outer one
          (let ((ovs (overlays-in beg end)))
            (dolist (ov ovs)
              (when (eql (overlay-get ov 'creator) 'yaml-pro)
                (delete-overlay ov))))
          (let ((ov (make-overlay beg end)))
            (overlay-put ov 'creator 'yaml-pro)
            (overlay-put ov 'invisible 'yaml-pro)
            (overlay-put ov 'isearch-open-invisible 'yaml-pro-isearch-show)
            (overlay-put ov 'isearch-open-invisible-temporary
                         (lambda (ov hide-p) (if hide-p (yaml-pro-hide-overlay ov)
                                               (yaml-pro-show-overlay ov))))
            (overlay-put ov 'display "...")
            (overlay-put ov 'face 'yaml-pro-fold-replacement-face)))))))

(defun yaml-pro-unfold-at-point ()
  "Unfold YAML at point."
  (interactive)
  (save-excursion
    (cond
     ((looking-at ".*:")
      (let ((ovs (overlays-in (point) (save-excursion (end-of-line) (1+ (point))))))
        (dolist (ov ovs)
          (when (eql (overlay-get ov 'creator) 'yaml-pro)
            (delete-overlay ov)))))
     (t
      (let ((ovs (overlays-at (1+ (point)))))
        (dolist (ov ovs)
          (when (eql (overlay-get ov 'creator) 'yaml-pro)
            (delete-overlay ov))))))))

(defun yaml-pro-up-level ()
  "Move the point to the parent tree."
  (interactive)
  (if (and (bolp) (not (looking-at "[ \n\t#]")))
      (goto-char (point-min))
    (let* ((start (point))
           (parse-tree (yaml-pro--get-buffer-tree))
           (bounds (yaml-pro-get-block-bounds parse-tree (point))))
      (goto-char (car bounds))
      (when (= start (point))
        (let* ((parse-tree (yaml-pro--get-buffer-tree))
               (bounds (yaml-pro-get-parent-block parse-tree (point))))
          (goto-char (car bounds))
          (when (= start (point))
            ;; the block we're at and it's parent have the same start,
            ;; move back one char and try again
            (forward-char -1)
            (yaml-pro-up-level))))
      (when (and (bolp) (looking-at "[ \t]"))
        ;; Don't let the command end at a block that begins a line
        (yaml-pro-up-level)))))

(defun yaml-pro-kill-subtree ()
  "Kill the entire subtree located at the current point."
  (interactive)
  (let* ((parse-tree (yaml-pro--get-buffer-tree))
         (bounds (yaml-pro-get-block-bounds parse-tree (point)))
         (start (car bounds))
         (end (cadr bounds)))
    (when (save-excursion
            (goto-char end)
            (looking-back "\n" (- (point) 2)))
      (setq end (1- end)))
    (kill-region start end)))

(defun yaml-pro-prev-subtree ()
  "Move the point to the previous sibling subtree."
  (interactive)
  (let* ((start-pos (point))
         (parse-tree (yaml-pro--get-buffer-tree))
         (at-bounds (yaml-pro-get-block-bounds parse-tree (point))))
    (goto-char (car at-bounds))
    (let ((at-col (current-column))
          (at-dash-p (looking-at "-")))
      (catch 'done
        (while (not (bobp))
          (forward-line -1)
          (while (looking-at "[ \n]*$")
            (forward-line -1))
          (if at-dash-p
              (skip-chars-forward " \n")
            (skip-chars-forward " \n-"))
          (when (and (< (current-column) at-col)
                     (not (looking-at "#")))
            (goto-char start-pos)
            (ding)
            (throw 'done nil))
          (when (and (= (current-column) at-col)
                     (not (looking-at "#")))
            (throw 'done t)))
        (if (looking-at "#")
            (progn
              (goto-char start-pos)
              (ding)
              nil)
          t)))))

(defun yaml-pro-next-subtree ()
  "Move the point to the next sibling subtree."
  (interactive)
  (let* ((start-pos (point))
         (start-col (current-column))
         (parse-tree (yaml-pro--get-buffer-tree))
         (at-bounds (yaml-pro-get-block-bounds parse-tree (point))))
    (goto-char (cadr at-bounds))
    (skip-chars-forward " \n")
    (if (or (not (= start-col (current-column)))
            (eobp))
        (progn
          (ding)
          (goto-char start-pos)
          nil)
      t)))

(defun yaml-pro-move-subtree-up ()
  "Swap the current subtree with the previous one."
  (interactive)
  (let* ((parse-tree (yaml-pro--get-buffer-tree))
         (at-bounds (yaml-pro-get-block-bounds parse-tree (point)))
         (at-contents (buffer-substring (car at-bounds) (cadr at-bounds)))
         (prev-bounds (save-excursion
                        (let ((ok (yaml-pro-prev-subtree)))
                          (and ok (yaml-pro-get-block-bounds parse-tree (point))))))
         (prev-contents (and prev-bounds (buffer-substring (car prev-bounds) (cadr prev-bounds)))))
    (when (not prev-bounds)
      (error "Can't move subtree up"))
    (goto-char (car at-bounds))
    (delete-region (car at-bounds) (cadr at-bounds))
    (insert prev-contents)
    (goto-char (car prev-bounds))
    (delete-region (car prev-bounds) (cadr prev-bounds))
    (insert at-contents)
    (goto-char (car prev-bounds))))

(defun yaml-pro-move-subtree-down ()
  "Swap the current subtree with the previous one."
  (interactive)
  (yaml-pro-next-subtree)
  (yaml-pro-move-subtree-up)
  (yaml-pro-next-subtree))

(defun yaml-pro--search-location (tree point path)
  "Return path up to POINT of TREE having visited at PATH."
  (cond
   ((stringp tree)
    (let ((pos (get-text-property 0 'yaml-position tree)))
      (when (<= (car pos) point (cdr pos))
        path)))
   (t
    (seq-find #'identity
              (seq-map
               (lambda (tuple)
                 (let* ((key (car tuple))
                        (key-pos (and (stringp key) (get-text-property 0 'yaml-position key)))
                        (val (cdr tuple))
                        (val-pos (and (stringp val) (get-text-property 0 'yaml-position val))))
                   (cond
                    ((and key-pos (<= (car key-pos) point (cdr key-pos)))
                     path)
                    ((and val-pos (<= (car val-pos) point (cdr val-pos)))
                     (cons key path))
                    ((vectorp val)
                     (catch 'found
                       (dotimes (i (length val))
                         (let* ((elt (aref val i))
                                (res (yaml-pro--search-location elt point (cons (number-to-string i) (cons key path)))))
                           (when res
                             (throw 'found res))))
                       nil))
                    ((listp val)
                     (yaml-pro--search-location val point (cons key path)))
                    (t nil))))
               tree)))))

(defconst yaml-pro-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      ;;(suppress-keymap map)
      (set-keymap-parent map yaml-mode-map)
      (define-key map (kbd "C-c C-x C-w") #'yaml-pro-kill-subtree)

      (define-key map (kbd "C-c C-u") #'yaml-pro-up-level)

      (define-key map (kbd "C-c C-p") #'yaml-pro-prev-subtree)
      (define-key map (kbd "C-c C-n") #'yaml-pro-next-subtree)

      (define-key map (kbd "C-c C-f") #'yaml-pro-fold-at-point)
      (define-key map (kbd "C-c C-o") #'yaml-pro-unfold-at-point)

      (define-key map (kbd "s-<up>") #'yaml-pro-move-subtree-up)
      (define-key map (kbd "s-<down>") #'yaml-pro-move-subtree-down)

      (define-key map (kbd "C-c '") #'yaml-pro-edit-scalar))))

;;;###autoload
(define-minor-mode yaml-pro-mode
  "Binds additional functions to aid in editing YAML files.

\\{yaml-pro-mode-map}"
  :init-value nil
  :group 'yaml-pro
  :keymap yaml-pro-mode-map
  (if yaml-pro-mode
      (progn
        (unless (fboundp 'yaml-parse-tree)
          (error "Unsupported yaml.el version.  Ensure that yaml.el package installed and at version 0.4"))
        (when (equal mode-name "YAML")
          (add-hook 'after-change-functions #'yaml-pro--after-change-hook nil t)))
    (remove-hook 'after-change-functions #'yaml-pro--after-change-hook t)))

(provide 'yaml-pro)

;;; yaml-pro.el ends here
