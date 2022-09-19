;;; yaml-pro.el --- Parser-aided YAML editing features -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.4
;; Package-Requires: ((emacs "26.1") (yaml "0.5.1"))
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
;; following commands are available:
;; - `yaml-pro-move-subtree-up' and `yaml-pro-move-subtree-down'
;; - `yaml-pro-next-subtree' and `yaml-pro-prev-subtree',
;; - `yaml-pro-kill-subtree'
;; - `yaml-pro-up-level'
;; - `yaml-pro-fold-at-point' and `yaml-pro-unfold-at-point'.
;; - `yaml-pro-edit-scalar'
;; - `yaml-pro-jump' or if consult exists `yaml-pro-consult-jump'

;;; Code:

(require 'yaml)
(require 'yaml-pro-edit)
(require 'consult nil t)

(defgroup yaml-pro nil
  "YAML editing tools."
  :prefix "yaml-pro-"
  :group 'convenience)

(defcustom yaml-pro-max-parse-size 5000
  "Size of buffer for which any size greater than use heuristic to parse.

Note that this isn't fully compatable with every command."
  :group 'yaml-pro
  :type 'number
  :package-version '(yaml-pro "0.2.0"))

(defface yaml-pro-fold-replacement-face
  '((t :inherit 'font-lock-comment-face))
  "Face for fold replacement text.")

(defvar yaml-pro-indent (if (boundp 'yaml-indent)
                            yaml-indent
                          2)
  "Default indentation to use for yaml-pro.")

(defvar-local yaml-pro-buffer-tree nil)

(defun yaml-pro--offset-parse-tree (tree offset)
  "Offset all TREE values of `yaml-position' property by OFFSET."
  (cond
   ((or (listp tree) (vectorp tree))
    (seq-map
     (lambda (elt)
       (yaml-pro--offset-parse-tree elt offset))
     tree))
   ((stringp tree)
    (let* ((yaml-position (get-text-property 0 'yaml-position tree)))
      (when yaml-position
        (let ((offset-position (cons (+ (car yaml-position) offset)
                                     (+ (cdr yaml-position) offset))))
          (set-text-properties 0
                               (length tree)
                               (list 'yaml-position offset-position)
                               tree)))))))

(defun yaml-pro--use-fast-p ()
  "Return non nil if buffer size is larger than `yaml-pro-max-parse-size'."
  (>= (buffer-size) yaml-pro-max-parse-size))

(defun yaml-pro--fast-parse-bounds (&optional point)
  "Return a list of the start and end point of subsection to parse.

Find subsection based off of POINT if provided."
  (let* ((point-indent
          (save-excursion
            (beginning-of-line)
            (skip-chars-forward " ")
            (current-column)))
         (indent-regexp (make-string point-indent ?\s))
         (start-point)
         (end-point))
    (if (= point-indent 0)
        (setq start-point (point-min)
              end-point (point-max))
      (save-excursion
        (beginning-of-line)
        (while (not (or (bobp)
                        (and (looking-at " *[a-zA-Z_0-9-]+:\\s-*$")
                             (not (looking-at indent-regexp)))))
          (forward-line -1))
        (setq start-point (point)))
      (save-excursion
        (beginning-of-line)
        (while (not (or (eobp)
                        (and (not (looking-at indent-regexp))
                             (not (looking-at " *$")))))
          (forward-line 1))
        (setq end-point (point))))
    (list start-point end-point)))

(defun yaml-pro--get-buffer-tree ()
  "Return the cached buffer-tree if exists, else regenerate it."
  (or yaml-pro-buffer-tree
      (if (not (yaml-pro--use-fast-p))
          (let ((tree (yaml-parse-tree (buffer-string))))
            (setq yaml-pro-buffer-tree tree)
            tree)
        (seq-let (start-point end-point) (yaml-pro--fast-parse-bounds)
          (let* ((subsection-string (buffer-substring start-point end-point))
                 (tree (yaml-parse-tree subsection-string)))
            (yaml-pro--offset-parse-tree tree (1- start-point))
            (when (and (= start-point (point-min))
                       (= end-point (point-max)))
              (setq yaml-pro-buffer-tree tree))
            tree)))))

(defun yaml-pro--after-change-hook (_ _ _)
  "Delete cached tree on buffer change."
  (setq yaml-pro-buffer-tree nil))

(defun yaml-pro--find-node (parse point)
  "Recursively look through PARSE to find scalar at POINT."
  (catch 'done
   (cond
    ((vectorp parse)
     (seq-do
      (lambda (item)
        (let ((res (yaml-pro--find-node item point)))
          (when res
            (throw 'done res))))
      parse)
     nil)
    ((listp parse)
     (dolist (item parse)
       (let ((res (yaml-pro--find-node (cdr item) point)))
         (when res
           (throw 'done res)))))
    ((stringp parse)
     (let* ((bounds (get-text-property 0 'yaml-position parse))
            (start (and bounds (car bounds)))
            (end (and bounds (cdr bounds))))
       (if (and start end (<= start point end))
           parse
         nil))))))

(defun yaml-pro--fast-value-at-point ()
  "Return the scalar under the current point.

This function uses a heuristic to limit the amount of parsing
that has to be done."

  (seq-let (start-point end-point) (yaml-pro--fast-parse-bounds)
    (let* ((subsection-string (buffer-substring start-point end-point))
           (parse (yaml-parse-string-with-pos subsection-string))
           (val (yaml-pro--find-node parse (1+ (- (point) start-point)))))
      (when val
        (let* ((yaml-position (get-text-property 0 'yaml-position val))
               (new-position (cons (1- (+ (car yaml-position) start-point))
                                   (1- (+ (cdr yaml-position) start-point)))))
          (set-text-properties 0 (length val) (list 'yaml-position new-position) val)
          val)))))

(defun yaml-pro--value-at-point ()
  "Return the scalar under the current point."
  (let* ((parse (yaml-parse-string-with-pos (buffer-string))))
    (yaml-pro--find-node parse (point))))

(defun yaml-pro--get-parent-block* (tree point)
  "Return subtree from TREE that best contain POINT."
  (if (not (listp tree))
      nil
    (let ((sub-blocks
           (seq-filter #'identity
                       (seq-map (lambda (st)
                                  (yaml-pro--get-parent-block* st point))
                                tree))))
      (cond
       ((and sub-blocks
             (stringp (car tree))
             (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
                    (start (car bounds))
                    (end (cdr bounds)))
               (and (numberp start) (<= start point end))))
        (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
               (start (car bounds))
               (end (cdr bounds)))
          (throw 'result (list start end))))
       (sub-blocks
        (car sub-blocks))
       ((stringp (car tree))
        (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
               (start (and bounds (car bounds)))
               (end (and bounds (cdr bounds))))
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
    (let ((sub-blocks
           (seq-filter #'identity
                       (seq-map (lambda (st) (yaml-pro-get-block-bounds st point))
                                tree))))
      (cond
       (sub-blocks
        ;; TODO should find best match instead of firt (?)
        (car sub-blocks))
       ((and (stringp (car tree)) (not (equal (car tree) "")))
        (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
               (start (and bounds (car bounds)))
               (end (and bounds (cdr bounds))))
          (if (and
               (numberp start)
               (<= start point end)
               (not (= (save-excursion
                         (goto-char start) (beginning-of-line) (point))
                       (save-excursion
                         (goto-char end) (beginning-of-line) (point)))))
              (list start end)
            nil)))
       (t nil)))))

(defun yaml-pro-get-block (tree point)
  "Return subtree from TREE that best contain POINT."
  (if (not (listp tree))
      nil
    (let ((sub-blocks
           (seq-filter #'identity
                       (seq-map (lambda (st) (yaml-pro-get-block st point))
                                tree))))
      (cond
       (sub-blocks
        ;; TODO should find best match instead of firt (?)
        (car sub-blocks))
       ((stringp (car tree))
        (let* ((bounds (get-text-property 0 'yaml-position (cadr tree)))
               (start (and bounds (car bounds)))
               (end (and bounds (cdr bounds))))
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
                        (key-pos (and (stringp key)
                                      (get-text-property 0 'yaml-position key)))
                        (val (cdr tuple))
                        (val-pos (and (stringp val)
                                      (get-text-property 0 'yaml-position val))))
                   (cond
                    ((and key-pos (<= (car key-pos) point (cdr key-pos)))
                     path)
                    ((and val-pos (<= (car val-pos) point (cdr val-pos)))
                     (cons key path))
                    ((vectorp val)
                     (catch 'found
                       (dotimes (i (length val))
                         (let* ((elt (aref val i))
                                (res (yaml-pro--search-location
                                      elt
                                      point
                                      (cons (number-to-string i)
                                            (cons key path)))))
                           (when res
                             (throw 'found res))))
                       nil))
                    ((listp val)
                     (yaml-pro--search-location val point (cons key path)))
                    (t nil))))
               tree)))))

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

(defun yaml-pro--flatten-tree (tree)
  "Return a \"flattened\" copy of TREE. Copied from Emacs 27.1."
  (let (elems)
    (while (consp tree)
      (let ((elem (pop tree)))
        (while (consp elem)
          (push (cdr elem) tree)
          (setq elem (car elem)))
        (if elem (push elem elems))))
    (if tree (push tree elems))
    (nreverse elems)))

(defun yaml-pro--extract-paths (tree &optional path)
  "Given TREE of parse, return all paths of tree to leaf nodes.

PATH is the current path we have already traversed down."
  (cond
   ((listp tree)
    (yaml-pro--flatten-tree
     (seq-map (lambda (key+val)
                (yaml-pro--extract-paths (cdr key+val)
                                         (append path (list (car key+val)))))
              tree)))
   ((vectorp tree)
    (yaml-pro--flatten-tree
     (seq-mapn
      (lambda (n val)
        (yaml-pro--extract-paths val (append path (list (format "[%d]" n)))))
      (number-sequence 0 (1- (length tree)))
      tree)))
   (t (concat (string-join path " > ") ": " tree))))

(defun yaml-pro-hide-overlay (ov)
  "Put fold-related properties on overlay OV."
  (overlay-put ov 'invisible 'yaml-pro)
  (overlay-put ov 'display "...")
  (overlay-put ov 'face 'yaml-pro-fold-replacement-face))

(defun yaml-pro-show-overlay (ov)
  "Remove fold-related properties of overlay OV."
  (overlay-put ov 'invisible nil)
  (overlay-put ov 'display nil)
  (overlay-put ov 'face nil))

(defun yaml-pro--get-last-yaml-pos (str)
  "Return the last-most text-property `yaml-position' of STR."
  (let ((i (1- (length str)))
        (pos))
    (while (or (not pos) (< i 0))
      (let ((at-pos (get-text-property i 'yaml-position str)))
        (when at-pos
          (setq pos at-pos))
        (cl-decf i)))
    pos))

(defun yaml-pro-jump ()
  "Jump to a specified path."
  (interactive)
  (let* ((tree (yaml-parse-string-with-pos (buffer-string)))
         (paths (yaml-pro--extract-paths tree))
         (selected (completing-read "Jump to: " paths nil t))
         ;; get original string which has properties intact
         (original-str (seq-find (lambda (s) (string= s selected)) paths))
         (pos (yaml-pro--get-last-yaml-pos original-str)))
    (goto-char (car pos))))

(declare-function consult--read "consult")
(declare-function consult--overlay "consult")
(declare-function consult--invisible-open-temporarily "consult")

(defun yaml-pro--consult-jump-preview ()
  "Return a consult function for displaying current selection."
  (let ((invisible)
        (overlays)
        (saved-pos (point-marker))
        (saved-min (point-min-marker))
        (saved-max (point-max-marker)))
    (lambda (cand restore)
      (mapc #'funcall invisible)
      (mapc #'delete-overlay overlays)
      (cond
       (restore
        (let ((saved-buffer (marker-buffer saved-pos)))
          (set-buffer saved-buffer)
          (narrow-to-region saved-min saved-max)
          (goto-char saved-pos)))
       (cand
        (let ((pos (and (not (string-blank-p cand))
                        (yaml-pro--get-last-yaml-pos cand))))
          (when pos
            (widen)
            (goto-char (car pos))
            (run-hooks 'consult-after-jump-hook)
            (setq invisible (consult--invisible-open-temporarily)
                  overlays
                  (list (consult--overlay (car pos) (cdr pos)
                                          'face 'consult-preview-cursor
                                          'window (selected-window)))))))))))

;;; Interactive commands

(defun yaml-pro-consult-jump ()
  "Jump to a specified path."
  (interactive)
  (let* ((tree (yaml-parse-string-with-pos (buffer-string)))
         (paths (yaml-pro--extract-paths tree))
         (sorted-paths (seq-sort-by (lambda (path)
                                      (car (yaml-pro--get-last-yaml-pos path)))
                                    #'< paths))
         (selected (consult--read
                    sorted-paths
                    :prompt "Jump to: "
                    :history 'yaml-pro-jump
                    :require-match t
                    :sort nil
                    :state (let ((preview (yaml-pro--consult-jump-preview)))
                             (lambda (str restore)
                               (funcall preview str restore)))))
         (original-str (seq-find (lambda (s) (string= s selected)) paths)))
    (goto-char (car (yaml-pro--get-last-yaml-pos original-str)))))

(defun yaml-pro-fold-at-point ()
  "Fold YAML at point."
  (interactive)
  (save-excursion
    (skip-syntax-forward " " (line-end-position))
    (let ((parse-tree (yaml-pro--get-buffer-tree)))
      (let* ((bounds (yaml-pro--fix-bounds
                      (yaml-pro-get-block parse-tree (point))))
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
                         (lambda (ov hide-p)
                           (if hide-p (yaml-pro-hide-overlay ov)
                             (yaml-pro-show-overlay ov))))
            (overlay-put ov 'display "...")
            (overlay-put ov 'face 'yaml-pro-fold-replacement-face)))))))

(defun yaml-pro-unfold-at-point ()
  "Unfold YAML at point."
  (interactive)
  (save-excursion
    (cond
     ((looking-at ".*:")
      (let ((ovs (overlays-in (point)
                              (save-excursion (end-of-line) (1+ (point))))))
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

(defun yaml-pro-indent-subtree ()
  "Indent the current subtree by one level.

Indentation is controlled by the variable `yaml-pro-indent'."
  (interactive)
  (let* ((parse-tree (yaml-pro--get-buffer-tree))
         (at-bounds (yaml-pro-get-block-bounds parse-tree (point))))
    (save-excursion
      (goto-char (car at-bounds))
      (if (not (looking-back "^[ ]*" nil))
          (progn
            (forward-line 0)
            (skip-chars-forward " ")
            (yaml-pro-indent-subtree))
        (let ((beginning-line (line-number-at-pos)))
          (goto-char (cadr at-bounds))
          (when (looking-back "\n" (- (point) 2))
            (forward-char -1))
          (forward-line 0)
          (while (>= (line-number-at-pos) beginning-line)
            (insert (make-string yaml-pro-indent ?\s))
            (forward-line -1)))))))

(defun yaml-pro-unindent-subtree ()
  "Unindent the current subtree by one level.

Indentation is controlled by the variable `yaml-pro-indent'."
  (interactive)
  (let* ((parse-tree (yaml-pro--get-buffer-tree))
         (at-bounds (yaml-pro-get-block-bounds parse-tree (point))))
    (save-excursion
      (goto-char (car at-bounds))
      ;; ensure that the subtree can be unintented
      (save-excursion
        (forward-line 0)
        (unless (looking-at-p (make-string yaml-pro-indent ?\s))
          (error "subtree can't be unintented further.")))
      (if (not (looking-back "^[ ]*" nil))
          (progn
            (forward-line 0)
            (skip-chars-forward " ")
            (yaml-pro-indent-subtree))
        (let ((beginning-line (line-number-at-pos)))
          (goto-char (cadr at-bounds))
          (when (looking-back "\n" (- (point) 2))
            (forward-char -1))
          (forward-line 0)
          (while (>= (line-number-at-pos) beginning-line)
            (when (looking-at-p (make-string yaml-pro-indent ?\s))
              (delete-char yaml-pro-indent))
            (forward-line -1)))))))

(defun yaml-pro-move-subtree-up ()
  "Swap the current subtree with the previous one."
  (interactive)
  (let* ((parse-tree (yaml-pro--get-buffer-tree))
         (at-bounds (yaml-pro-get-block-bounds parse-tree (point)))
         (at-contents (buffer-substring (car at-bounds) (cadr at-bounds)))
         (prev-bounds (save-excursion
                        (let ((ok (yaml-pro-prev-subtree)))
                          (and ok (yaml-pro-get-block-bounds parse-tree
                                                             (point))))))
         (prev-contents (and prev-bounds
                             (buffer-substring (car prev-bounds)
                                               (cadr prev-bounds)))))
    (when (not prev-bounds)
      (error "Can't move subtree"))
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

(defconst yaml-pro-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-c C-x C-w") #'yaml-pro-kill-subtree)

      (define-key map (kbd "C-c C-u") #'yaml-pro-up-level)

      (define-key map (kbd "C-c C-p") #'yaml-pro-prev-subtree)
      (define-key map (kbd "C-c C-n") #'yaml-pro-next-subtree)

      (define-key map (kbd "C-c C-f") #'yaml-pro-fold-at-point)
      (define-key map (kbd "C-c C-o") #'yaml-pro-unfold-at-point)

      (define-key map (kbd "s-<up>") #'yaml-pro-move-subtree-up)
      (define-key map (kbd "s-<down>") #'yaml-pro-move-subtree-down)

      (define-key map (kbd "C-c '") #'yaml-pro-edit-scalar)

      (define-key map (kbd "C-c >") #'yaml-pro-indent-subtree)

      (define-key map (kbd "C-c <") #'yaml-pro-unindent-subtree)

      (define-key map (kbd "C-c C-j") (if (featurep 'consult)
                                          #'yaml-pro-consult-jump
                                        #'yaml-pro-jump)))))

(defconst yaml-pro-required-yaml-parser-version "0.5.1")

;;;###autoload
(define-minor-mode yaml-pro-mode
  "Binds additional functions to aid in editing YAML files.

\\{yaml-pro-mode-map}"
  :init-value nil
  :group 'yaml-pro
  :keymap yaml-pro-mode-map
  (if yaml-pro-mode
      (progn
        (when (or (not (boundp 'yaml-parser-version))
                  (not yaml-parser-version)
                  (version< yaml-parser-version
                            yaml-pro-required-yaml-parser-version))
          (error "Unsupported yaml.el version.  \
Ensure that yaml.el package installed and at version %s"
                 yaml-pro-required-yaml-parser-version))
        (when (equal mode-name "YAML")
          (add-hook 'after-change-functions #'yaml-pro--after-change-hook nil t)))
    (remove-hook 'after-change-functions #'yaml-pro--after-change-hook t)))

(provide 'yaml-pro)

;;; yaml-pro.el ends here
