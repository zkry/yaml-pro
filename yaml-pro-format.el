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

;; yaml-pro-format-ts is a command which will format the current
;; buffer to make have uniform styling.  The rules the formatter
;; applies are defined in the `yaml-pro-format-features' variable.  By
;; changing its value you can enable and disable formatting rules.
;;
;; The pretty formatter was inspired by the prettier.js YAML formatter
;; but does have a number of differences, some limited by the
;; capabilities of Tree Sitter.

;;; Code:

(require 'treesit nil t)
(require 'cl-lib)

(defvar yaml-pro-indent)

(defcustom yaml-pro-format-print-width 80
  "Width until which to break flow sequences."
  :group 'yaml-pro
  :type 'integer)

(defcustom yaml-pro-format-features
  '(reduce-newlines document-separator-own-line oneline-flow block-formatting
                    reduce-spaces bm-fn-next-line clean-doc-end remove-spaces-before-comments
                    expand-long-flow single-to-double indent)
  "Features to enable when formatting buffer.

This value should be a list containing any of the following symbols:

- `indent' When present, indent each line by `yaml-pro-indent'.

- `reduce-newlines' When present, remove adjacent newlines so at
  most one one remains.  New lines in strings won't be removed.

- `oneline-flow' When present, reduce flow mappings to one line.

- `block-formatting' When present, format the spacing around
  block components' colons and dashes.

- `reduce-spaces' When present, attempt to reduce multiple
  adjacent space characters down to one.

- `bm-fn-next-line' When present, move the value of a key-value pair to the
  next line, indented, if it's width is longer than
  `yaml-pro-format-print-width'.

- `expand-log-flow' When present, flatten flow elements that pass
  column `yaml-pro-format-print-width'.

- `single-to-double' When present, convert single quoted strings
  to double quoted, when it wouldn't change its meaning.

- `remove-spaces-before-comments' When present, remove spaces
  before a comment until it is one space after the preceeding
  content (e.g. \"a: b   # space before here\").

- `clean-doc-end' When present, remove unnecessary document-end
  indicators (\"...\").

- `document-separator-own-line' When present, makes sure the
  document separator \"---\" isn't on a line with another element."
  :group 'yaml-pro
  :type '(set (const :tag "Remove adjacent blank lines, leaving at most one"
                     reduce-newlines)
              (const :tag "Put document separators (---) are on a line of their own"
                     document-separator-own-line)
              (const :tag "Try to reduce flow (JSON-like) elements to one line"
                     oneline-flow)
              (const :tag "Format spacing around block elments' colon and dashes"
                     block-formatting)
              (const :tag "Reduce spacing down to one when appropriate"
                     reduce-spaces)
              (const :tag "Transform \"key: value\" to \"key:\\n  value\" if column passes `yaml-pro-format-print-width'"
                     bm-fn-next-line)
              (const :tag "Remove unnecessary document end indicator (...)"
                     clean-doc-end)
              (const :tag "Leave only one space before comments"
                     remove-spaces-before-comments)
              (const :tag "Flatten flow nodes if column passes `yaml-pro-format-print-width'"
                     expand-long-flow)
              (const :tag "Convert single-quoted strings to double-quoted strings"
                     single-to-double)
              (const :tag "Indent according to treesitter level information"
                     indent)))

(defun yaml-pro-format-ts--bm-single-space ()
  "Ensure proper spacing around the colon character in a block mapping."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node 'yaml)
                 '((block_mapping_pair
                    key: (flow_node) @key
                    ":" @colon
                    value: [(flow_node) (block_node)] @val))))
         (del-ovs '()))
    (while nodes
      (save-excursion
        (let* ((colon-node (alist-get 'colon nodes))
               (key-node (alist-get 'key nodes))
               (val-node (alist-get 'val nodes)))
          (setq nodes (cdddr nodes))
          (when (and (not (= (treesit-node-start colon-node)
                             (treesit-node-end key-node)))
                     ;; we should only be overwriting blank here
                     (string-match-p "\\`[ \n]*\\'"
                                     (buffer-substring-no-properties
                                      (treesit-node-end key-node)
                                      (treesit-node-start colon-node)))
                     ;; alias names need a space after them
                     (not (equal (treesit-node-type
                                  (treesit-node-at (treesit-node-end key-node) 'yaml))
                                 "alias_name")))
            (push (make-overlay (treesit-node-end key-node)
                                (treesit-node-start colon-node))
                  del-ovs))
          (when
              (and (or (string-suffix-p "_scalar" (treesit-node-type val-node))
                       (equal (treesit-node-type val-node) "flow_node"))
                   (not (= (1+ (treesit-node-end colon-node))
                           (treesit-node-start val-node)))
                   ;; the region should only replace blank
                   (string-match-p
                    "\\`[ \n]*\\'"
                    (buffer-substring-no-properties
                     (treesit-node-end colon-node)
                     (treesit-node-start val-node))))
            (let ((ov (make-overlay (treesit-node-end colon-node)
                                    (treesit-node-start val-node))))
              (overlay-put ov 'yaml-pro-format-insert " ")
              (push ov del-ovs))))))
    del-ovs))

(defun yaml-pro-format-ts--oneline-flow ()
  "Compress flow elements to one line."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node 'yaml)
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
      ;; don't compress flow with comments
      (unless (treesit-query-capture node '((comment) @comment))
        (save-match-data
          (save-excursion
            (goto-char (treesit-node-start node))
            (while (search-forward "\n" (treesit-node-end node) t)
              (unless (equal (treesit-node-type
                              (treesit-node-at (match-beginning 0) 'yaml))
                             "comment")
                (let* ((ov (make-overlay (match-beginning 0) (match-end 0))))
                  (push ov del-ovs))))))))
    ;; Following code ensures that compressed flow is pretty formatted
    (let* ((nodes (treesit-query-capture
                   (treesit-buffer-root-node 'yaml)
                   '((flow_mapping "," @comma)))))
      (pcase-dolist (`(_ . ,comma-node) nodes)
        (cond
         ((save-excursion (goto-char (treesit-node-end comma-node))
                          (looking-at-p " *\n? *}"))
          (let* ((ov (make-overlay (treesit-node-start comma-node)
                                   (treesit-node-end comma-node))))
            (overlay-put ov 'yaml-pro-format-insert " ")
            (push ov del-ovs)))
         ((save-excursion (goto-char (treesit-node-start comma-node))
                          (looking-back "[^\n] *" (- (point) 80)))
          (let* ((ov (make-overlay (save-excursion
                                     (goto-char (treesit-node-start comma-node))
                                     (skip-chars-backward " ")
                                     (point))
                                   (treesit-node-start comma-node))))
            (push ov del-ovs))))))
    (let* ((nodes (treesit-query-capture
                   (treesit-buffer-root-node 'yaml)
                   '((flow_mapping "}" @rbrace)))))
      (pcase-dolist (`(_ . ,rbrace-node) nodes)
        (when (save-excursion (goto-char (treesit-node-start rbrace-node))
                              (not (looking-back "[ \n{] ?" (- (point) 4))))
          (let* ((ov (make-overlay (treesit-node-start rbrace-node)
                                   (treesit-node-start rbrace-node))))
            (overlay-put ov 'yaml-pro-format-insert " ")
            (push ov del-ovs)))))
    (let* ((nodes (treesit-query-capture
                   (treesit-buffer-root-node 'yaml)
                   '((flow_mapping "{" @lbrace)))))
      (pcase-dolist (`(_ . ,lbrace-node) nodes)
        (when (save-excursion (goto-char (treesit-node-end lbrace-node))
                              (not (looking-at-p "[ \n}]")))
          (let* ((ov (make-overlay (treesit-node-end lbrace-node)
                                   (treesit-node-end lbrace-node))))
            (overlay-put ov 'yaml-pro-format-insert " ")
            (push ov del-ovs)))))
    del-ovs))


(defun yaml-pro-format-ts--no-comments-after-tags ()
  "Move comments to a newline if they occur after a tag (e.g. !!set)."
  (let* ((del-ovs '())
         (nodes (treesit-query-capture
                 (treesit-buffer-root-node 'yaml)
                 '((_ (tag) @tag :anchor (comment) @comment)))))
    (while nodes
      (let* ((tag-node (alist-get 'tag nodes))
             (comment-node (alist-get 'comment nodes)))
        (setq nodes (cddr nodes))
        (when (= (line-number-at-pos (treesit-node-end tag-node))
                 (line-number-at-pos (treesit-node-start comment-node)))
          (let* ((ov (make-overlay (treesit-node-end tag-node)
                                   (treesit-node-start comment-node))))
            (overlay-put ov 'yaml-pro-format-insert "\n")
            (push ov del-ovs)))))
    del-ovs))

(defun yaml-pro-format-ts--flow-seq-grouping-space ()
  "Remove all whitespace before and after flow nodes' [ ] chars."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node 'yaml)
                 '((flow_mapping) @flow
                   (flow_sequence) @flow)))
         (del-ovs '()))
    (pcase-dolist (`(_ . ,node) nodes)
      (let-alist (treesit-query-capture
                  node
                  '((flow_sequence "[" @open :anchor (_) @after-open-node
                                   (_) @before-close-node :anchor "]" @close)))
        (when (and .open .close .after-open-node .before-close-node)
          (if (= (line-number-at-pos (treesit-node-end .open))
                 (line-number-at-pos (treesit-node-start .close)))
              ;; If [ ] is on one line, remove spacing after [ and before ].
              (let* ((ov-beg (make-overlay (treesit-node-end .open)
                                           (treesit-node-start .after-open-node)))
                     (ov-end (make-overlay (treesit-node-end .before-close-node)
                                           (treesit-node-start .close))))
                (push ov-beg del-ovs)
                (unless (equal (treesit-node-type .before-close-node) "comment")
                  (push ov-end del-ovs)))
            ;; If [ ] is on multiple lines, ensure [ ] are on their own lines.
            (let* ((after-open-indent
                    (yaml-pro-format-ts--node-indent .after-open-node))
                   (ov-beg (make-overlay (treesit-node-end .open)
                                         (treesit-node-start .after-open-node)))
                   (ov-end (make-overlay (treesit-node-end .before-close-node)
                                         (treesit-node-start .close))))
              (overlay-put ov-beg 'yaml-pro-format-insert
                           (concat "\n" (make-string
                                         (* after-open-indent yaml-pro-indent)
                                         ?\s)))
              (overlay-put ov-end 'yaml-pro-format-insert
                           (concat ",\n" (make-string
                                          (* (- after-open-indent 1)
                                             yaml-pro-indent)
                                          ?\s)))
              (push ov-beg del-ovs)
              (unless (equal (treesit-node-type .before-close-node) "comment")
                (push ov-end del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--bm-fn-next-line ()
  "If a flow node passes the 80th column, put it on the next line."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node 'yaml)
                 '((block_mapping_pair
                     ":" @colon :anchor value: (flow_node) @child)
                    @parent)))
         (del-ovs '()))
    (while nodes
      (let-alist (seq-take nodes 3)
        (setq nodes (cdddr nodes))
        (when (and (= (line-number-at-pos (treesit-node-start .parent))
                      (line-number-at-pos (treesit-node-start .child)))
                   (> (save-excursion (goto-char (treesit-node-end .child))
                                      (current-column))
                      80))
          (let* ((indent (yaml-pro-format-ts--node-indent .child))
                 (ov (make-overlay (treesit-node-end .colon)
                                   (treesit-node-start .child))))
            (overlay-put
             ov 'yaml-pro-format-insert
             (concat "\n" (make-string (* indent yaml-pro-indent) ?\s)))
            (push ov del-ovs)))))
    del-ovs))

(defun yaml-pro-format-ts--clean-doc-end ()
  "Remove the document end symbol (...)."
  (let* ((del-ovs '())
         (capture (treesit-query-capture
                   (treesit-buffer-root-node 'yaml)
                   '(("..." @doc-end)))))
    (pcase-dolist (`(_ . ,node) capture)
      (save-excursion
        (goto-char (treesit-node-end node))
        (skip-chars-forward "\n \t")
        (let* ((next-node (treesit-node-at (point) 'yaml)))
          (cond
           ((member (treesit-node-type next-node) '("comment" "directive_name"))
            ;; leave alone if next node is a comment
            )
           ((equal (treesit-node-type next-node) "---")
            (let* ((ov (make-overlay (1- (treesit-node-start node))
                                     (treesit-node-end node))))
              (push ov del-ovs)))
           (t
            (let* ((ov (make-overlay (treesit-node-start node)
                                     (treesit-node-end node))))
              (overlay-put ov 'yaml-pro-format-insert "---")
              (push ov del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--remove-spaces-before-comments ()
  "Remove spaces before comments.
If there is nothing before the comment on a line, move it to col 0.
If there is other text, ensure one space remains."
  (let* ((del-ovs '())
         (capture (treesit-query-capture
                   (treesit-buffer-root-node 'yaml)
                   '((comment) @comment))))
    (pcase-dolist (`(_ . ,comment-node) capture)
      (save-excursion
        (save-match-data
          (goto-char (treesit-node-start comment-node))
          (cond
           ((looking-back "\n *" (- (point) 80))
            (let* ((ov (make-overlay (pos-bol) (treesit-node-start comment-node))))
              (push ov del-ovs)))
           ((looking-back "[^\n] *" (- (point) 80))
            (let* ((ov (make-overlay
                        (save-excursion (goto-char (treesit-node-start comment-node))
                                        (skip-chars-backward " \t")
                                        (point))
                                     (treesit-node-start comment-node))))
              (overlay-put ov 'yaml-pro-format-insert " ")
              (push ov del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--expand-long-flow-sequence ()
  "Expand flow sequence to multiple lines if extend past column 80.
Assumes that flow sequences have been previously reduced to one line."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node 'yaml)
                 '((flow_sequence) @seq)))
         (ovs '()))
    (pcase-dolist (`(_ . ,node) nodes)
      (when (save-excursion (goto-char (treesit-node-end node))
                            (and (looking-at-p ",? *\n")
                                 (> (current-column) yaml-pro-format-print-width)))
        (let* ((capture (treesit-query-capture
                         node
                         '((flow_sequence (flow_node) @child)))))
          (pcase-dolist (`(_ . ,child) capture)
            (when (treesit-node-eq (treesit-node-parent child) node)
              (let* ((indent (yaml-pro-format-ts--node-indent child))
                     (ov (make-overlay
                          (treesit-node-start child) (treesit-node-start child))))
                (overlay-put
                 ov 'yaml-pro-format-insert
                 (concat "\n" (make-string (* indent yaml-pro-indent) ?\s)))
                (push ov ovs)))))
        (pcase-dolist (`(_ . ,close) (treesit-query-capture
                                      node
                                      '((flow_sequence "]" @close :anchor))))
          (when (treesit-node-eq (treesit-node-parent close) node)
            (let* ((ov (make-overlay (treesit-node-start close)
                                     (treesit-node-start close)))
                   (indent (yaml-pro-format-ts--node-indent close))
                   (end-str "\n"))
              (when (not (save-excursion (goto-char (treesit-node-start close))
                                         (looking-back ", *" (- (point) 80))))
                (setq end-str ",\n"))
              (overlay-put ov 'yaml-pro-format-insert
                           (concat end-str
                                   (make-string (* indent yaml-pro-indent)
                                                ?\s)))
              (push ov ovs))))))
    ovs))

(defun yaml-pro-format-ts--expand-long-flow-mapping ()
  "Expand flow mapping to multiple lines if extend past column 80.
Assumes that flow mappings have been previously reduced to one line."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node 'yaml)
                 '((flow_mapping) @map)))
         (ovs '()))
    (pcase-dolist (`(_ . ,node) nodes)
      (when (save-excursion
              (goto-char (treesit-node-end node))
              (and (looking-at-p ",? *\n")
                   (or (> (current-column) yaml-pro-format-print-width)
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
                         (not (looking-back "\n *" (- (point) 80)))))
              (let* ((indent (yaml-pro-format-ts--node-indent child))
                     (ov (make-overlay (treesit-node-start child)
                                       (treesit-node-start child))))
                (overlay-put ov 'yaml-pro-format-insert
                             (concat "\n" (make-string (* indent yaml-pro-indent)
                                                       ?\s)))
                (push ov ovs)))))
        (let* ((capture (treesit-query-capture
                         node
                         '((flow_mapping "}" @close :anchor)))))
          (pcase-dolist (`(_ . ,close) capture)
            (when (treesit-node-eq (treesit-node-parent close) node)
              (let* ((ov (make-overlay (save-excursion
                                         (goto-char (treesit-node-start close))
                                         (skip-chars-backward " ")
                                         (point))
                                       (treesit-node-start close)))
                     (indent (yaml-pro-format-ts--node-indent close))
                     (end-str "\n"))
                ;; we only add this ov if flow is on same line
                (unless (save-excursion (goto-char (treesit-node-start close))
                                        (looking-back "\n *" (- (point) 80)))
                  (when (not (save-excursion (goto-char (treesit-node-start close))
                                             (looking-back ", *" (- (point) 10))))
                    (setq end-str ",\n"))

                  (overlay-put
                   ov 'yaml-pro-format-insert
                   (concat end-str (make-string (* indent yaml-pro-indent) ?\s)))
                  (push ov ovs))))))))
    ovs))

(defun yaml-pro-format-ts--block-sequence ()
  "Ensure proper spacing around sequences' - character."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node 'yaml)
                 `((block_sequence_item "-" @dash (_) @elt))))
         (groups (seq-group-by
                  (lambda (capture)
                    (treesit-node-parent (cdr capture)))
                  nodes))
         (del-ovs '()))
    (while groups
      (save-excursion
        (let* ((dash-node (alist-get 'dash (cdar groups)))
               (elt-node (alist-get 'elt (cdar groups))))
          (let ((ov (make-overlay (treesit-node-end dash-node)
                                  (treesit-node-start elt-node))))
            (overlay-put ov 'yaml-pro-format-insert " ")
            (push ov del-ovs)))
        (setq groups (cdr groups))))
    del-ovs))

(defun yaml-pro-format-ts--single-to-double ()
  "Convert single quote if it doesn't contain backslashes."
  (let* ((nodes (treesit-query-capture
                 (treesit-buffer-root-node 'yaml)
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
            (let* ((open-ov (make-overlay (treesit-node-start node)
                                          (1+ (treesit-node-start node))))
                   (close-ov (make-overlay (1- (treesit-node-end node))
                                           (treesit-node-end node))))
              (overlay-put open-ov 'yaml-pro-format-insert "\"")
              (overlay-put close-ov 'yaml-pro-format-insert "\"")
              (push open-ov del-ovs)
              (push close-ov del-ovs))))))
    del-ovs))

(defun yaml-pro-format-ts--reduce-spaces ()
  "Remove multiple spaces when not affecting meaning."
  (let* ((del-ovs '()))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t][ \t]+" nil t)
          (when (not (looking-back "\n *" (- (point) 80)))
            (let* ((at-node-type (treesit-node-type (treesit-node-on
                                                     (match-beginning 0)
                                                     (match-end 0)
                                                     'yaml))))
              (when (not (member at-node-type
                                 '("string_scalar" "double_quote_scalar"
                                   "single_quote_scalar" "block_scalar"
                                   "block_sequence_item" "comment")))
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
      (let* ((at-node (treesit-node-at (point) 'yaml)))
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
          (let* ((on-node-type
                  (treesit-node-type (treesit-node-on (match-beginning 0)
                                                      (match-end 0)
                                                      'yaml))))
            (when (and (not (member
                             on-node-type '("string_scalar" "double_quote_scalar"
                                            "single_quote_scalar" "block_scalar")))
                       (not (yaml-pro-format-ts--after-keep-p (point))))
              (let* ((ov (make-overlay (match-beginning 0 ) (match-end 0))))
                (overlay-put ov 'yaml-pro-format-insert "\n\n")
                (push ov del-ovs)))))))
    del-ovs))

(defun yaml-pro-format-ts--document-separator-own-line ()
  "If document contans ---, make sure it's on its own line."
  (let* ((del-ovs '())
         (capture (treesit-query-capture (treesit-buffer-root-node 'yaml)
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

(defun yaml-pro-format-ts--commenting-next-node (node)
  "If NODE is a comment, return the node next in the tree if commenting it.
Often times a comment block will be commenting on something below
it.  We infer this if a comment is part of a block where there is
a blank line above it."
  (when (equal (treesit-node-type node) "comment")
    (let* ((comment-block-start node)
           (comment-block-end node))
      (catch 'out
       (while t
         (let* ((next-sib (treesit-node-next-sibling comment-block-end)))
           (if (and (equal (treesit-node-type next-sib) "comment")
                    (= (line-number-at-pos (treesit-node-start next-sib))
                       (1+ (line-number-at-pos (treesit-node-start comment-block-end)))))
               (setq comment-block-end next-sib)
             (throw 'out nil)))))
      (catch 'out
       (while t
         (let* ((prev-sib (treesit-node-prev-sibling comment-block-start)))
           (if (and (equal (treesit-node-type prev-sib) "comment")
                    (= (line-number-at-pos (treesit-node-start prev-sib))
                       (1- (line-number-at-pos (treesit-node-start comment-block-start)))))
               (setq comment-block-start prev-sib)
             (throw 'out nil)))))
      (when (save-excursion
              (goto-char (treesit-node-start comment-block-start))
              (forward-line -1)
              (looking-at-p " *\n"))
        (let* ((next-block-node
                (save-excursion
                  (goto-char (treesit-node-start comment-block-end))
                  (forward-line 1)
                  (back-to-indentation)
                  (and (looking-at-p " *[^ \n\t-]")
                       (treesit-node-at (point) 'yaml)))))
          (when (not (equal (treesit-node-type next-block-node) "comment"))
            next-block-node))))))

(defun yaml-pro-format-ts--node-indent (root-node)
  "Return the number of indent parents of ROOT-NODE."
  (let* ((ct 0)
         (node root-node))
    ;; base count calculation
    (while node
      (when (and (equal (treesit-node-field-name node) "key")
                 (not (equal (treesit-node-type (treesit-node-parent node))
                             "flow_pair")))
        (cl-decf ct))
      (when (member (treesit-node-type node) '("-" "{" "[" "}" "]"))
        (cl-decf ct))
      (when (member (treesit-node-type node)
                    '("block_mapping_pair" "block_sequence_item"
                      "flow_mapping" "flow_sequence"))
        (cl-incf ct))
      (setq node (treesit-node-parent node)))

    ;; When a non-numbered block scalar is at indent level-0, indent
    ;; body contents.
    (when (and (= 0 ct)
               (equal (treesit-node-type root-node) "block_scalar")
               (not (yaml-pro-format-ts--node-numbered-block-scalar-p root-node)))
      (cl-incf ct))

    ;; Block scalars' text that is past the most indent needs to keep
    ;; its extra indentation
    (when (and (equal (treesit-node-type root-node) "block_scalar")
               (> (current-column)
                  (yaml-pro-format-ts--lowest-block-scalar-indent root-node)))
      (cl-incf ct (/ (- (current-column)
                        (yaml-pro-format-ts--lowest-block-scalar-indent root-node))
                     yaml-pro-indent)))
    ct))

(defun yaml-pro-format-ts--should-indent-p (node)
  "Return non-nil if NODE should be indented according to tree parse."
  (cond
   ((yaml-pro-format-ts--node-numbered-block-scalar-p node) nil)
   ((equal (treesit-node-type node) ":") nil)
   (t t)))

(defun yaml-pro-format-ts--indent ()
  "Indent elements according to tree position."
  (let* ((del-ovs '()))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (search-forward-regexp "\\(\n *\\)[^ \n]" nil t)
          (forward-char -1)
          (let* ((at-node (treesit-node-at (point) 'yaml)))
            (when (yaml-pro-format-ts--should-indent-p at-node)
              ;; in certain cases, a comment should be indented by the node
              ;; after it.
              (setq at-node (or (yaml-pro-format-ts--commenting-next-node at-node)
                                at-node))
              (let* ((indent (yaml-pro-format-ts--node-indent at-node))
                     (indent-str
                      (make-string (* indent yaml-pro-indent) ?\s))
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
  (let* ((capture (treesit-query-capture
                   (treesit-buffer-root-node 'yaml)
                   '((block_sequence_item "-") @item)))
         (groups (seq-group-by (pcase-lambda (node)
                                 (treesit-node-parent node))
                               (seq-map #'cdr capture))))
    (pcase-dolist (`(_ . ,items) groups)
      (when (> (length items) 1)
        (push (seq-map
               (pcase-lambda (node)
                 (make-overlay (treesit-node-start node)
                               (treesit-node-end node) nil t))
               items)
              yaml-pro-format-ts-indent-groups))))
  (let* ((capture (treesit-query-capture
                   (treesit-buffer-root-node 'yaml)
                   '((block_mapping_pair ":") @item)))
         (groups (seq-group-by (pcase-lambda (node)
                                 (treesit-node-parent node))
                               (seq-map #'cdr capture))))
    (pcase-dolist (`(_ . ,items) groups)
      (when (> (length items) 1)
        (push (seq-map (pcase-lambda (node)
                         (make-overlay (treesit-node-start node)
                                       (treesit-node-end node) nil t))
                       items)
              yaml-pro-format-ts-indent-groups)))))

(defun yaml-pro-format-ts--ensure-indent-groups ()
  "Ensure that indent groups are on same level of indentation."
  (let* ((ovs '()))
    (dolist (group yaml-pro-format-ts-indent-groups)
      (let* ((indent (save-excursion (goto-char (overlay-start (car group)))
                                     (current-column))))
        (dolist (node (cdr group))
          (let* ((node-indent (save-excursion (goto-char (overlay-start node))
                                              (current-column)))
                 (indent-diff (- indent node-indent)))
            (cond
             ((< indent-diff 0)
              (let* ((ov (make-overlay (+ (overlay-start node) indent-diff)
                                       (overlay-start node))))
                (push ov ovs)))
             ((< 0 indent-diff)
              (let* ((ov (make-overlay (overlay-start node)
                                       (overlay-start node))))
                (overlay-put ov 'yaml-pro-format-insert
                             (make-string indent-diff ?\s))
                (push ov ovs))))))))
    ovs))

(defconst yaml-pro-format-ts-functions
  '((reduce-newlines . yaml-pro-format-ts--reduce-newlines)
    (document-separator-own-line . yaml-pro-format-ts--document-separator-own-line)
    (oneline-flow . yaml-pro-format-ts--oneline-flow)
    (block-formatting . yaml-pro-format-ts--bm-single-space)
    ((or oneline-flow expand-flow) . yaml-pro-format-ts--flow-seq-grouping-space)
    (reduce-spaces . yaml-pro-format-ts--reduce-spaces)
    (block-formatting . yaml-pro-format-ts--block-sequence)
    (bm-fn-next-line . yaml-pro-format-ts--bm-fn-next-line)
    (clean-doc-end . yaml-pro-format-ts--clean-doc-end)
    (remove-spaces-before-comments . yaml-pro-format-ts--remove-spaces-before-comments)
    (expand-long-flow .
                      (lambda ()
                        (yaml-pro-format-ts--run-while-changing
                         '(yaml-pro-format-ts--expand-long-flow-sequence
                           yaml-pro-format-ts--expand-long-flow-mapping))))
    (single-to-double .
                      yaml-pro-format-ts--single-to-double)
    ;; yaml-pro-format-ts--prose-wrap TODO
    (indent . yaml-pro-format-ts--indent))
  "Alist of feature symbol and formatting functions.")


(defun yaml-pro-format ()
  "Format the YAML buffer according to pre-defined rules."
  (interactive)
  (when (treesit-query-capture (treesit-buffer-root-node 'yaml) '((ERROR) @err))
    (user-error "Error encountered when parsing buffer with treesitter.  Please ensure your syntax is correct and try again"))
  (save-excursion
    (goto-char (point-max))
    (insert "\n"))
  (yaml-pro-format-ts--create-indent-groups)
  (let* ((fmt-functions
          (seq-map
           #'cdr
           (seq-filter
            (pcase-lambda (`(,sym . _))
              (pcase sym
                (`(or . ,elts)
                 (seq-find (lambda (sym) (member sym yaml-pro-format-features))
                           elts))
                (sym (member sym yaml-pro-format-features))))
            yaml-pro-format-ts-functions))))
    (dolist (f fmt-functions)
      (let* ((ovs (funcall f)))
        (save-excursion
          (seq-map #'yaml-pro-format-ts--process-overlay ovs)))
      (let* ((ovs (yaml-pro-format-ts--ensure-indent-groups)))
        (save-excursion
          (seq-map #'yaml-pro-format-ts--process-overlay ovs))))
    (delete-trailing-whitespace (point-min) (point-max))))

(defalias 'yaml-pro-format-ts #'yaml-pro-format)

(provide 'yaml-pro-format)

;;; yaml-pro-format.el ends here
