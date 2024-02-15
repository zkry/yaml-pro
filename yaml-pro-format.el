;;; yaml-pro-format.el --- Pretty formatting for YAML code -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.4
;; Package-Requires: ((emacs "28.1"))
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
;; - [x] Bring flow onto one line
;; - [x] flow_mapping past column X should be broken
;; - [x] flow_sequence past column X should be broken
;; - [ ] single quotes should change to doulbe quotes if they don't have backslash
;; - [ ] trailing space deleted

(defcustom yaml-pro-format-indent 2
  "Amount of spaces to indent YAML."
  :group 'yaml-pro
  :type 'integer)

(defcustom yaml-pro-format-prose-wrap t
  "Wrap block strings to one line if meaning stays the same."
  :group 'yaml-pro
  :type 'boolean)

(defun yaml-pro-format-ts--bm-single-space ()
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((block_mapping_pair key: (flow_node) value: [(flow_node) (block_node)]) @bm)))
         (del-ovs '()))
    (save-excursion
      (pcase-dolist (`(_ . ,node) nodes)
        (let* ((child-nodes (treesit-query-capture
                             node
                             '((block_mapping_pair key: (flow_node) @key
                                                   ":" @colon
                                                   value: [(flow_node) (block_node)] @val))))
               (child-nodes (seq-filter (pcase-lambda (`(_ . ,n)) ;; TODO - will I need this pattern in other places?
                                               (treesit-node-eq (treesit-node-parent n)
                                                                node))
                                             child-nodes))
               (colon-node (alist-get 'colon child-nodes))
               (key-node (alist-get 'key child-nodes))
               (val-node (alist-get 'val child-nodes)))
          (when (and (not (= (treesit-node-start colon-node)
                             (treesit-node-end key-node)))
                     ;; we should only be overwriting blank here
                     (string-match-p "\\`[ \n]*\\'" (buffer-substring-no-properties (treesit-node-end key-node) (treesit-node-start colon-node)))
                     ;; alias names need a space after them
                     (not (equal (treesit-node-type (treesit-node-at (treesit-node-end key-node)))
                                 "alias_name")))
            (push (make-overlay (treesit-node-end key-node) (treesit-node-start colon-node)) del-ovs))

          (when (and (or (string-suffix-p "_scalar" (treesit-node-type val-node))
                         (equal (treesit-node-type val-node) "flow_node"))
                     (not (= (1+ (treesit-node-end colon-node))
                             (treesit-node-start val-node)))
                     ;; the region should only replace blank
                     (string-match-p "\\`[ \n]*\\'" (buffer-substring-no-properties (treesit-node-end colon-node) (treesit-node-start val-node))))

            (let ((ov (make-overlay (treesit-node-end colon-node)
                                    (treesit-node-start val-node))))
              (overlay-put ov 'yaml-pro-format-insert " ")
              (push ov del-ovs))))))
    ;; (pcase-dolist (`(_ . ,node) (treesit-query-capture
    ;;                              (treesit-buffer-root-node)
    ;;                              '((block_mapping_pair key: (_) :anchor ":") @bmp)))
    ;;   (let-alist (treesit-query-capture node '((block_mapping_pair key: (_) @key :anchor ":" @colon)))
    ;;     (when (string-match-p
    ;;            " +"
    ;;            (buffer-substring-no-properties (treesit-node-end .key) (treesit-node-start .colon)))
    ;;       (let* ((ov (make-overlay (treesit-node-end .key) (treesit-node-start .colon))))
    ;;         (push ov del-ovs)))))
    del-ovs))

(defun yaml-pro-format-ts--oneline-flow ()
  ""
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((document
                    (flow_node
                     [(flow_mapping) (flow_sequence)] @flow))
                   (block_mapping_pair
                    (flow_node
                     [(flow_mapping) (flow_sequence)] @flow))
                   (block_sequence_item
                    (flow_node
                     [(flow_mapping) (flow_sequence)] @flow)))))
         (del-ovs '()))
    (pcase-dolist (`(_ . ,node) nodes)
      (save-match-data
        (save-excursion
          (goto-char (treesit-node-start node))
          (while (search-forward "\n" (treesit-node-end node) t)
            (unless (equal (treesit-node-type (treesit-node-at (match-beginning 0))) "comment")
              (let* ((ov (make-overlay (match-beginning 0) (match-end 0))))
                (push ov del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--no-comments-after-tags ()
  "Move comments to a newline if they occur after a tag (e.g. !!set)."
  (let* ((del-ovs '())
         (nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((_ (tag) @tag :anchor (comment) @comment)))))
    (while nodes
      (let* ((tag-node (alist-get 'tag nodes))
             (comment-node (alist-get 'comment nodes)))
        (setq nodes (cddr nodes))
        (when (= (line-number-at-pos (treesit-node-end tag-node))
                 (line-number-at-pos (treesit-node-start comment-node)))
          (let* ((ov (make-overlay (treesit-node-end tag-node) (treesit-node-start comment-node))))
            (overlay-put ov 'yaml-pro-format-insert "\n")
            (push ov del-ovs)))))
    del-ovs))

(defun yaml-pro-format-ts--flow-groupings-nospace ()
  "Remove all whitespace before and after flow nodes' [ ] { } chars."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((flow_mapping) @flow
                   (flow_sequence) @flow)))
         (del-ovs '()))
    (pcase-dolist (`(_ . ,node) nodes)
      (dolist (mapping (list (treesit-query-capture
                               node
                               '((flow_mapping  "{" @open :anchor (_) @after-open-node
                                                (_) @before-close-node :anchor "}" @close)))
                              (treesit-query-capture
                               node
                               '((flow_sequence "[" @open :anchor (_) @after-open-node
                                                (_) @before-close-node :anchor "]" @close)))))
        (let-alist mapping
          (when (and .open .close .after-open-node .before-close-node)
            (let* ((ov-beg (make-overlay (treesit-node-end .open) (treesit-node-start .after-open-node)))
                   (ov-end (make-overlay (treesit-node-end .before-close-node) (treesit-node-start .close))))
              (push ov-beg del-ovs)
              (unless (equal (treesit-node-type .before-close-node) "comment")
                (push ov-end del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--bm-fn-next-line ()
  "If a flow node passes the 80th column, put it on the next line."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((block_mapping_pair ":" :anchor value: (flow_node)) @node)))
         (del-ovs '()))
    (pcase-dolist (`(_ . ,node) nodes)
      (let-alist (treesit-query-capture
                  node
                  '((block_mapping_pair ":" @colon :anchor value: (flow_node) @child) @parent))
        (when (and (= (line-number-at-pos (treesit-node-start .parent))
                      (line-number-at-pos (treesit-node-start .child)))
                   (> (save-excursion (goto-char (treesit-node-end .child)) (current-column)) 80))
          (let* ((indent (yaml-pro-format-ts--node-indent .child))
                 (ov (make-overlay (treesit-node-end .colon) (treesit-node-start .child))))
            (overlay-put ov 'yaml-pro-format-insert (concat "\n" (make-string (* indent yaml-pro-format-indent) ?\s)))
            (push ov del-ovs)))))
    del-ovs))

(defun yaml-pro-format-ts--clean-doc-end ()
  "Remove the document end symbol (...)."
  (let* ((del-ovs '())
         (capture (treesit-query-capture
                   (treesit-buffer-root-node)
                   '(("..." @doc-end)))))
    (pcase-dolist (`(_ . ,node) capture)
      (save-excursion
        (goto-char (treesit-node-end node))
        (skip-chars-forward "\n \t")
        (let* ((next-node (treesit-node-at (point))))
          (cond
           ((equal (treesit-node-type next-node) "comment")
            ;; leave alone if next node is a comment
            )
           ((equal (treesit-node-type next-node) "---")
            (let* ((ov (make-overlay (1- (treesit-node-start node)) (treesit-node-end node))))
              (push ov del-ovs)))
           (t
            (let* ((ov (make-overlay (treesit-node-start node) (treesit-node-end node))))
              (overlay-put ov 'yaml-pro-format-insert "---")
              (push ov del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--expand-long-flow-sequence ()
  "Expand flow sequence to multiple lines if extend past column 80.
Assumes that flow sequences have been previously reduced to one line."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((flow_sequence) @seq)))
         (ovs '()))
    (pcase-dolist (`(_ . ,node) nodes)
      (when (save-excursion (goto-char (treesit-node-end node))
                            (and (looking-at-p ", *\n")
                                 (> (current-column) 80)))
        (let* ((capture (treesit-query-capture
                         node
                         '((flow_sequence (flow_node) @child)))))
          (pcase-dolist (`(_ . ,child) capture)
            (when (treesit-node-eq (treesit-node-parent child) node)
              (let* ((indent (yaml-pro-format-ts--node-indent child))
                     (ov (make-overlay (treesit-node-start child) (treesit-node-start child))))
                (overlay-put ov 'yaml-pro-format-insert (concat "\n" (make-string (* indent yaml-pro-format-indent) ?\s)))
                (push ov ovs)))))
        (pcase-dolist (`(_ . ,close) (treesit-query-capture
                                      node
                                      '((flow_sequence "]" @close :anchor))))
          (when (treesit-node-eq (treesit-node-parent close) node)
            (let* ((ov (make-overlay (treesit-node-start close) (treesit-node-start close)))
                   (indent (yaml-pro-format-ts--node-indent close))
                   (end-str "\n"))
              (when (not (save-excursion (goto-char (treesit-node-start close))
                                         (looking-back ", *" (- (point) 8))))
                (setq end-str ",\n"))
              (overlay-put ov 'yaml-pro-format-insert (concat end-str (make-string (* indent yaml-pro-format-indent) ?\s)))
              (push ov ovs))))))
    ovs))

(defun yaml-pro-format-ts--expand-long-flow-mapping ()
  "Expand flow mapping to multiple lines if extend past column 80.
Assumes that flow mappings have been previously reduced to one line."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((flow_mapping) @map)))
         (ovs '()))
    (pcase-dolist (`(_ . ,node) nodes)
      (when (save-excursion (goto-char (treesit-node-end node))
                            (and (looking-at-p ",? *\n")
                                 (or (> (current-column) 80)
                                     ;; if the flow is already spanning multiple lines, expand
                                     ;; it so it looks nice.
                                     (not (= (line-number-at-pos (treesit-node-start node))
                                             (line-number-at-pos (treesit-node-end node)))))))
        (let* ((capture (treesit-query-capture
                         node
                         '((flow_mapping [(flow_pair) (flow_node)] @child)))))
          (pcase-dolist (`(_ . ,child) capture)
            (when (and (treesit-node-eq (treesit-node-parent child) node)
                       (save-excursion ;; dont add newline if one already exists
                         (goto-char (treesit-node-start child))
                         (not (looking-back "\n *" (- (point) 20)))))
              (let* ((indent (yaml-pro-format-ts--node-indent child))
                     (ov (make-overlay (treesit-node-start child) (treesit-node-start child))))
                (overlay-put ov 'yaml-pro-format-insert (concat "\n" (make-string (* indent yaml-pro-format-indent) ?\s)))
                (push ov ovs)))))
        (let* ((capture (treesit-query-capture
                         node
                         '((flow_mapping "}" @close :anchor)))))
          (pcase-dolist (`(_ . ,close) capture)
            (when (treesit-node-eq (treesit-node-parent close) node)
              (let* ((ov (make-overlay (treesit-node-start close) (treesit-node-start close)))
                     (indent (yaml-pro-format-ts--node-indent close))
                     (end-str "\n"))
                (unless (save-excursion (goto-char (treesit-node-start close))  ; we only add this ov
                                        (looking-back "\n *" (- (point) 10)))   ; if flow is on same line
                  (when (not (save-excursion (goto-char (treesit-node-start close))
                                             (looking-back ", *" (- (point) 10))))
                    (setq end-str ",\n"))

                  (overlay-put ov 'yaml-pro-format-insert (concat end-str (make-string (* indent yaml-pro-format-indent) ?\s)))
                  (push ov ovs))))))))
    ovs))

(defun yaml-pro-format-ts--block-sequence ()
  ""
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((block_sequence_item "-" (_)) @sequence)))
         (del-ovs '()))
    (save-excursion
      (pcase-dolist (`(_ . ,node) nodes)
        (let* ((child-nodes (treesit-query-capture node `((block_sequence_item "-" @dash (_) @elt))))
               (child-nodes (seq-filter (pcase-lambda (`(_ . ,n))
                                               (treesit-node-eq (treesit-node-parent n)
                                                                node))
                                             child-nodes))
               (dash-node (alist-get 'dash child-nodes))
               (elt-node (alist-get 'elt child-nodes)))
          (let ((ov (make-overlay (treesit-node-end dash-node)
                                  (treesit-node-start elt-node))))
            (overlay-put ov 'yaml-pro-format-insert " ")
            (push ov del-ovs)))))
    del-ovs))

(defun yaml-pro-format-ts--single-to-double ()
  "Convert single quote if it doesn't contain backslashes."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node)
                 '((single_quote_scalar) @scalar)))
         (del-ovs '()))
    (pcase-dolist (`(_ . ,node) nodes)
      (save-excursion
        (save-restriction
          (narrow-to-region (treesit-node-start node) (treesit-node-end node))
          (goto-char (treesit-node-start node))
          (unless (search-forward "\\" nil t)
            ;; single quotes are to use backslash w/o escaping.  If
            ;; single scalar has backslash, don't convert.
            (while (search-forward "''" nil t)
              (let* ((ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put ov 'yaml-pro-format-insert "'")
                (push ov del-ovs)))
            (let* ((open-ov (make-overlay (treesit-node-start node) (1+ (treesit-node-start node))))
                   (close-ov (make-overlay (1- (treesit-node-end node)) (treesit-node-end node))))
              (overlay-put open-ov 'yaml-pro-format-insert "\"")
              (overlay-put close-ov 'yaml-pro-format-insert "\"")
              (push open-ov del-ovs)
              (push close-ov del-ovs))))))
    del-ovs))

;; (defun yaml-pro-format-ts--flow-pair-end-with-comma ()
;;   "Add a comma to the end of flow key-values, even if not needed."
;;   (let* ((del-ovs '())
;;          (capture (treesit-query-capture (treesit-buffer-root-node) '((flow_pair) @pair))))
;;     (pcase-dolist (`(_ . ,node) capture)
;;       (save-excursion
;;         (goto-char (treesit-node-end node))
;;         (unless (looking-at-p " *,")
;;           (let* ((ov (make-overlay (treesit-node-end node) (treesit-node-end node))))
;;             (overlay-put ov 'yaml-pro-format-insert ",")
;;             (push ov del-ovs)))))
;;     del-ovs))

(defun yaml-pro-format-ts--reduce-spaces ()
  (let* ((del-ovs '()))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t][ \t]+" nil t)
          (when (not (looking-back "\n *" (- (point) 30)))
            (let* ((at-node-type (treesit-node-type (treesit-node-on (match-beginning 0) (match-end 0)))))
              (when (not (member at-node-type '("string_scalar" "double_quote_scalar" "single_quote_scalar" "block_scalar" "block_sequence_item")))
                (let* ((ov (make-overlay (match-beginning 0) (match-end 0))))

                  (overlay-put ov 'yaml-pro-format-insert " ")
                  (push ov del-ovs))))))))
    del-ovs))

(defun yaml-pro-format-ts--after-keep-p (point)
  "Return non-nil of POINT is after a block scalar keep item.
This is needed due to a mismatch of the TreeSitter grammer and the YAML spec."
  (save-excursion
    (save-match-data
      (goto-char point)
      (while (looking-back "[\n ]" (- (point) 2))
        (forward-char -1))
      (let* ((at-node (treesit-node-at (point))))
        (and (equal (treesit-node-type at-node)
                    "block_scalar")
             (yaml-pro-format-ts--node-keep-block-scalar-p at-node))))))

(defun yaml-pro-format-ts--reduce-newlines ()
  "Reduce all appropriate grouped empty lines into one."
  (let* ((del-ovs '()))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward-regexp "\\(?:\n[ \t]*\\)\\(?:\n[ \t]*\\)+\n" nil t)
          (let* ((on-node-type (treesit-node-type (treesit-node-on (match-beginning 0) (match-end 0)))))
            (when (and (not (member on-node-type '("string_scalar" "double_quote_scalar" "single_quote_scalar" "block_scalar")))
                       (not (yaml-pro-format-ts--after-keep-p (point))))
              (let* ((ov (make-overlay (match-beginning 0 ) (match-end 0))))
                (overlay-put ov 'yaml-pro-format-insert "\n\n")
                (push ov del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--document-separator-own-line ()
  "If document contans ---, make sure it's on its own line."
  (let* ((del-ovs '())
         (capture (treesit-query-capture (treesit-buffer-root-node)
                                         '((document "---" @dashes)))))
    (pcase-dolist (`(_ . ,node) capture)
      (save-excursion
        (save-match-data
          (goto-char (treesit-node-end node))
          (when (looking-at "\\( *\\)[^ \n]")
            (let* ((ov (make-overlay (match-beginning 1) (match-end 1))))
              (overlay-put ov 'yaml-pro-format-insert "\n")
              (push ov del-ovs))))))
    del-ovs))

(defun yaml-pro-format-ts--node-numbered-block-scalar-p (node)
  "Return non-nil if NODE is a block scalar with an indent count."
  (and (equal (treesit-node-type node) "block_scalar")
       (string-match-p "^[>|][0-9]+" (treesit-node-text node))))

(defun yaml-pro-format-ts--node-keep-block-scalar-p (node)
  "Return non-nil if NODE is a block scalar with an indent count."
  (and (equal (treesit-node-type node) "block_scalar")
       (string-match-p "^[>|][0-9]*\\+" (treesit-node-text node))))

(defun yaml-pro-format-ts--lowest-block-scalar-indent (node)
  "Return the lowest indentation of text in NODE."
  (save-match-data
    (apply #'min
           (cdr (seq-map (lambda (line)
                           (when (string-match "^ *" line)
                             (length (match-string 0 line))))
                         (string-lines (treesit-node-text node) t))))))

(defun yaml-pro-format-ts--node-indent (root-node)
  "Return the number of indent parents of ROOT-NODE."
  (let* ((ct 0)
         (node root-node))
    ;; base count calculation
    (while node
      (when (and (equal (treesit-node-field-name node) "key")
                 (not (equal (treesit-node-type (treesit-node-parent node)) "flow_pair")))
        (cl-decf ct))
      (when (member (treesit-node-type node) '("-" "{" "[" "}" "]"))
        (cl-decf ct))
      (when (member (treesit-node-type node) '("block_mapping_pair" "block_sequence_item" "flow_mapping" "flow_sequence"))
        (cl-incf ct))
      (setq node (treesit-node-parent node)))

    ;; When a non-numbered block scalar is at indent level-0, indent body contents.
    (when (and (= 0 ct)
               (equal (treesit-node-type root-node) "block_scalar")
               (not (yaml-pro-format-ts--node-numbered-block-scalar-p root-node)))
      (cl-incf ct))

    ;; Block scalars' text that is past the most indent needs to keep its extra indentation
    (when (and (equal (treesit-node-type root-node) "block_scalar")
               (> (current-column)
                  (yaml-pro-format-ts--lowest-block-scalar-indent root-node)))
      (cl-incf ct (/ (- (current-column)
                        (yaml-pro-format-ts--lowest-block-scalar-indent root-node))
                     yaml-pro-format-indent)))
    ct))

(defun yaml-pro-format-ts--should-indent-p (node)
  "Return non-nil if NODE should be indented according to tree parse."
  (cond
   ((yaml-pro-format-ts--node-numbered-block-scalar-p node) nil)
   ((equal (treesit-node-type node) ":") nil)
   (t t)))

(defun yaml-pro-format-ts--indent ()
  ""
  (let* ((del-ovs '()))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward-regexp "\\(\n *\\)[^ \t\n\r]" nil t)
          (forward-char -1)
          (let* ((at-node (treesit-node-at (point))))
            (when (yaml-pro-format-ts--should-indent-p at-node)
              (let* ((indent (yaml-pro-format-ts--node-indent at-node))
                     (indent-str (make-string (* indent yaml-pro-format-indent) ?\s))
                     (ov (make-overlay (1+ (match-beginning 0)) (point))))
                (overlay-put ov 'yaml-pro-format-insert indent-str)
                (push ov del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--process-overlay (ov)
  "Delete characters in OV.  Insert text of property `yaml-pro-format-insert'.
OV is deleted after this function finishes."
  (let* ((insert-text (overlay-get ov 'yaml-pro-format-insert)))
    (goto-char (overlay-start ov))
    (delete-region (overlay-start ov) (overlay-end ov))
    (when insert-text
      (insert insert-text))
    (delete-overlay ov)))

(defun yaml-pro-format-ts--run-while-changing (functions)
  "Run FUNCTIONS in order.  Keep running FUNCTIONS until no change made."
  (catch 'done
    (while t
      (let ((changed nil))
        (dolist (f functions)
          (let* ((ovs (funcall f)))
            (when (> (length ovs) 0)
              (setq changed t))
            (save-excursion
              (seq-map #'yaml-pro-format-ts--process-overlay ovs))))
        (if (not changed)
            (throw 'done nil)
          (setq changed nil))))))

(defvar-local yaml-pro-format-ts-indent-groups '()
  "List of nodes that need to be indented together at the same time.")

(defun yaml-pro-format-ts--create-indent-groups ()
  "Populate `yaml-pro-format-ts-indent-groups' based on initial tree."
  (setq yaml-pro-format-ts-indent-groups nil)
  (let* ((capture (treesit-query-capture (treesit-buffer-root-node) '((block_sequence) @seq))))
    (pcase-dolist (`(_ . ,seq-node) capture)
      (let* ((items (treesit-query-capture (treesit-buffer-root-node) '((block_sequence_item "-") @item))))
        (setq items (seq-filter (pcase-lambda (`(_ . ,item-node))
                                  (treesit-node-eq (treesit-node-parent item-node) seq-node))
                                items))
        (when (> (length items) 1)
          (push (seq-map (pcase-lambda (`(_ . ,node))
                           (make-overlay (treesit-node-start node) (treesit-node-end node) nil t))
                         items)
                yaml-pro-format-ts-indent-groups))))))

(defun yaml-pro-format-ts--ensure-indent-groups ()
  "Ensure that indent groups are on same level of indentation."
  (let* ((ovs '()))
    (dolist (group yaml-pro-format-ts-indent-groups)
      (let* ((indent (save-excursion (goto-char (overlay-start (car group))) (current-column))))
        (dolist (node (cdr group))
          (let* ((node-indent (save-excursion (goto-char (overlay-start node)) (current-column)))
                 (indent-diff (- indent node-indent)))
            (cond
             ((< indent-diff 0)
              (let* ((ov (make-overlay (+ (overlay-start node) indent-diff) (overlay-start node))))
                (push ov ovs)))
             ((< 0 indent-diff)
              (let* ((ov (make-overlay (overlay-start node) (overlay-start node))))
                (overlay-put ov 'yaml-pro-format-indent (make-string indent-diff ?\s))
                (push ov ovs))))))))
    ovs))

(defun yaml-pro-format-ts ()
  (interactive)
  (save-excursion ;; TODO - dont depend on this
    (goto-char (point-max))
    (insert "\n"))
  (yaml-pro-format-ts--create-indent-groups)
  (let* ((fmt-functions '(yaml-pro-format-ts--reduce-newlines
                          yaml-pro-format-ts--document-separator-own-line
                          yaml-pro-format-ts--oneline-flow
                          ;; yaml-pro-format-ts--no-comments-after-tags
                          yaml-pro-format-ts--bm-single-space
                          yaml-pro-format-ts--flow-groupings-nospace
                          yaml-pro-format-ts--reduce-spaces
                          yaml-pro-format-ts--block-sequence
                          yaml-pro-format-ts--bm-fn-next-line
                          yaml-pro-format-ts--clean-doc-end
                          (lambda ()
                            (yaml-pro-format-ts--run-while-changing
                             '(yaml-pro-format-ts--expand-long-flow-sequence
                               yaml-pro-format-ts--expand-long-flow-mapping)))
                          yaml-pro-format-ts--single-to-double
                          ;; yaml-pro-format-ts--prose-wrap TODO
                          yaml-pro-format-ts--indent)))
    (dolist (f fmt-functions)
      (let* ((ovs (funcall f)))
        (save-excursion
          (seq-map #'yaml-pro-format-ts--process-overlay ovs)))
      (let* ((ovs (yaml-pro-format-ts--ensure-indent-groups)))
        (save-excursion
          (seq-map #'yaml-pro-format-ts--process-overlay ovs))))))

(provide 'yaml-pro-format)

;;; yaml-pro-format.el ends here
