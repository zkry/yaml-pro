;;; yaml-pro-format.el --- Pretty formatting for YAML code -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.4
;; Package-Requires: ((emacs "26.1"))
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

;; yaml-pro-format is a command which will format the current buffer
;; to make have uniform styling.
;;
;; TODO - write up styling changes
;;

;;; Code:

(require 'treesit)
(require 'cl-lib)


;; - [x] Formatting around ":" in block mapping
;; - [x] Formatting after "-" in block sequence
;; - [x] Multiple spaces reduced to one space (e.g. `["a",     "b", "c"]`)
;; - [x] Empty lines are reduced down to one empty line
;; - [x] Only 2 spaces for indentation
;;   - [x] Block scalars, if they don't have a indent spec should be minimally indented.
;;   - [x] Multi-line strings should be minimally indented if they span multiple lines.
;;   - [ ] Dont mess with |# or >#
;; - [x] Comments should indent to the elements after them
;; - [ ] flow_mapping past column X should be broken
;; - [ ] flow_sequence past column X should be broken
;; - [ ] single quotes should change to doulbe quotes if they don't have backslash
;; - [ ] trailing space deleted

(defun yaml-pro-format-ts--single-space ()
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((block_mapping_pair key: (flow_node) value: (flow_node)) @bm)))
         (del-ovs '()))
    (save-excursion
      (pcase-dolist (`(_ . ,node) nodes)
        (let* ((child-nodes (treesit-query-capture
                             node
                             '((block_mapping_pair key: (flow_node) @key
                                                   ":" @paren
                                                   value: (flow_node) @val))))
               (paren-node (alist-get 'paren child-nodes))
               (key-node (alist-get 'key child-nodes))
               (val-node (alist-get 'val child-nodes)))
          (when (not (= (treesit-node-start paren-node)
                        (treesit-node-end key-node)))
            (push (make-overlay (treesit-node-end key-node) (treesit-node-start paren-node)) del-ovs))
          (when (and (string-suffix-p "_scalar" (treesit-node-type val-node))
                     (not (= (1+ (treesit-node-end paren-node))
                             (treesit-node-start val-node))))
            (let ((ov (make-overlay (treesit-node-end paren-node)
                                    (treesit-node-start val-node))))
              (overlay-put ov 'yaml-pro-format-insert " ")
              (push ov del-ovs))))))
    del-ovs))

(defun yaml-pro-format-ts--block-sequence ()
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((block_sequence_item "-" (_)) @sequence)))
         (del-ovs '()))
    (save-excursion
      (pcase-dolist (`(_ . ,node) nodes)
        (let* ((child-nodes (treesit-query-capture node `((block_sequence_item "-" @dash (_) @elt))))
               (dash-node (alist-get 'dash child-nodes))
               (elt-node (alist-get 'elt child-nodes))
               (ov (make-overlay (treesit-node-end dash-node)
                                 (treesit-node-start elt-node))))
          (overlay-put ov 'yaml-pro-format-insert " ")
          (push ov del-ovs))))
    del-ovs))

(defun yaml-pro-format-ts--reduce-spaces ()
  (let* ((del-ovs '()))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t][ \t]+" nil t)
          (when (not (looking-back "\n *" (- (point) 30)))
            (let* ((at-node-type (treesit-node-type (treesit-node-on (match-beginning 0) (match-end 0)))))
              (when (not (member at-node-type '("string_scalar" "double_quote_scalar" "single_quote_scalar" "block_scalar")))
                (let* ((ov (make-overlay (match-beginning 0) (match-end 0))))

                  (overlay-put ov 'yaml-pro-format-insert " ")
                  (push ov del-ovs))))))))
    del-ovs))

(defun yaml-pro-format-ts--reduce-newllines ()
  (let* ((del-ovs '()))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward-regexp "\\(?:\n[ \t]*\\)\\(?:\n[ \t]*\\)+\n" nil t)
          (let* ((on-node-type (treesit-node-on (match-beginning 0) (match-end 0))))
            (when (not (member on-node-type '("string_scalar" "double_quote_scalar" "single_quote_scalar" "block_scalar")))
              (let* ((ov (make-overlay (match-beginning 0 ) (match-end 0))))
                (overlay-put ov 'yaml-pro-format-insert "\n\n")
                (push ov del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--node-indent (node)
  "Return the number of indent parents of NODE."
  (let* ((ct 0))
    (while node
      (when (and (equal (treesit-node-field-name node) "key")
                 (not (equal (treesit-node-type (treesit-node-parent node)) "flow_pair")))
        (cl-decf ct))
      (when (member (treesit-node-type node) '("-" "{" "["))
        (cl-decf ct))
      (when (member (treesit-node-type node) '("block_mapping" "block_sequence" "flow_mapping" "flow_sequence"))
        (cl-incf ct))
      (setq node (treesit-node-parent node)))
    ct))

(defcustom yaml-pro-format-indent 2
  "Amount of spaces to indent YAML."
  :group 'yaml-pro
  :type 'integer)

(defun yaml-pro-format-ts--indent ()
  ""
  (let* ((del-ovs '()))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward-regexp "\\(\n +\\)[^ \t\n\r]" nil t)
          (forward-char -1)
          (let* ((at-node (treesit-node-at (point)))
                 (indent (yaml-pro-format-ts--node-indent at-node))
                 (indent-str (make-string (* indent yaml-pro-format-indent) ?\s))
                 (ov (make-overlay (1+ (match-beginning 0)) (point))))
            (overlay-put ov 'yaml-pro-format-insert indent-str)
            (push ov del-ovs)))))
    del-ovs))

(defun yaml-pro-format-ts ()
  (interactive)
  (let* ((fmt-functions '(yaml-pro-format-ts--block-sequence
                          yaml-pro-format-ts--single-space
                          yaml-pro-format-ts--reduce-newllines
                          yaml-pro-format-ts--reduce-spaces
                          yaml-pro-format-ts--indent)))
    (dolist (f fmt-functions)
      (let* ((ovs (funcall f)))
        (save-excursion
          (dolist (ov ovs)
            (let* ((insert-text (overlay-get ov 'yaml-pro-format-insert)))
              (goto-char (overlay-start ov))
              (delete-region (overlay-start ov) (overlay-end ov))
              (when insert-text
                (insert insert-text))
              (delete-overlay ov))))))))

(provide 'yaml-pro-format)

;;; yaml-pro-format.el ends here
