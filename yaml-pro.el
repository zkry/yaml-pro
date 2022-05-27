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

(defvar yaml-pro-tree nil)

(require 'yaml)

(defun yaml-pro-get-block (tree point)
  (if (not (listp tree))
      nil
    (let ((sub-blocks (seq-filter #'identity
                                  (seq-map (lambda (st) (yaml-pro-get-block st point))
                                           tree))))
      (cond
       (sub-blocks
        ;; TODO should find best match instead of firt (?)
        (car sub-blocks))
       ((and (eql (car tree) 'yaml-position)
             (<= (1+ (nth 1 tree)) point (1+ (nth 2 tree))))
        (list (1+ (nth 1 tree)) (1+ (nth 2 tree))))
       (t nil)))))

(defun yaml-pro--fix-bounds (bounds)
  (seq-let (beg end) bounds
    (save-excursion
      (goto-char beg)
      (cond
       ((and (not (looking-at "{"))
             (not (looking-at "\\[")))
        (end-of-line)
        (setq beg (point)))))
    (save-excursion
      (goto-char end)
      (cond
       ((looking-back "\n" (- (point) 2))
        (setq end (1- end)))))
    (list beg end)))

(defun yaml-pro-fold-at-point ()
  "Toggle YAML fold at point."
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-syntax-forward " " (line-end-position))
    (let ((parse-tree (yaml-parse-tree (buffer-string))))
      (let* ((bounds (yaml-pro--fix-bounds (yaml-pro-get-block parse-tree (point))))
             (beg (car bounds))
             (end (cadr bounds))
             (ov (make-overlay beg end)))
        (overlay-put ov 'creator 'yaml-pro)
        (overlay-put ov 'invisible 'yaml-pro)
        (overlay-put ov 'display "...")
        (overlay-put ov 'face 'origami-fold-replacement-face)))))

(provide 'yaml-pro)

;;; yaml-pro.el ends here
