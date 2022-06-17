;;; yaml-pro.el --- slick yaml editing features -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ((yaml "0.4.0"))
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
  "Return the object path to current point."
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
        (let ((parse-tree (yaml-pro--get-buffer-tree))
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
      (define-key map (kbd "s-<down>") #'yaml-pro-move-subtree-down))))

;;;###autoload
(define-minor-mode yaml-pro-mode
  "Binds additional functions to aid in editing YAML files.

\\{yaml-pro-mode-map}"
  :init-value nil
  :group 'yaml-pro
  :keymap yaml-pro-mode-map
  (if yaml-pro-mode
      (progn
        (when (equal mode-name "YAML")
          (add-hook 'after-change-functions #'yaml-pro--after-change-hook nil t)))
    (remove-hook 'after-change-functions #'yaml-pro--after-change-hook t)))

(provide 'yaml-pro)

;;; yaml-pro.el ends here
