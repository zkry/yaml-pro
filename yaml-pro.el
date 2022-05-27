;;; yaml-pro.el --- slick yaml editing features -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: (())
;; Homepage: https://github.com/zkry/yaml-pro.el
;; Keywords: ?


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

;; no

;;; Code:

(require 'yaml)

(defgroup yaml-pro nil
  "YAML editing tools."
  :prefix "yaml-pro-"
  :group 'convenience)

(defface yaml-pro-fold-replacement-face
  '((t :inherit 'font-lock-comment-face))
  "Face for fold replacement text.")

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
        (message ">>> %d" (length sub-blocks))
        (car sub-blocks))
       ((and (eql (car tree) 'yaml-position)
             (<= (1+ (nth 1 tree)) point (1+ (nth 2 tree)))
             ;; hack to get small maps to not get selected
             (yaml-pro--fix-bounds (list (1+ (nth 1 tree)) (1+ (nth 2 tree)))))
        (list (1+ (nth 1 tree)) (1+ (nth 2 tree))))
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
        (setq beg (point)))))
    (save-excursion
      (goto-char end)
      (cond
       ((looking-back "\n" (- (point) 2))
        (setq end (1- end)))))
    (if (= beg end)
        nil
      (list beg end))))

(defun yaml-pro-fold-at-point ()
  "Fold YAML at point."
  (interactive)
  (save-excursion
    (skip-syntax-forward " " (line-end-position))
    (let ((parse-tree (yaml-parse-tree (buffer-string))))
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

(define-key yaml-mode-map (kbd "C-c C-c") #'yaml-pro-fold-at-point)
(define-key yaml-mode-map (kbd "C-c C-o") #'yaml-pro-unfold-at-point)

(provide 'yaml-pro)

;;; yaml-pro.el ends here
