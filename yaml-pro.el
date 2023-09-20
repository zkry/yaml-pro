;;; yaml-pro.el --- Parser-aided YAML editing features -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.3.1
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
(require 'treesit nil t)
(require 'consult nil t)

(defgroup yaml-pro nil
  "YAML editing tools."
  :prefix "yaml-pro-"
  :group 'convenience)


;;; yaml-pro tree sitter

(defcustom yaml-pro-ts-yank-subtrees t
  "Non-nil means when yanking subtrees, adjust the level."
  :group 'yaml-pro
  :type 'boolean)

(defun yaml-pro-ts--until-mapping (node)
  "Recursively look up from NODE returning first `block_mapping_pair'."
  (treesit-parent-until
   node
   (lambda (node)
     (equal (treesit-node-type node) "block_mapping_pair"))))

(defun yaml-pro-ts--until-list (node)
  "Recursively look up from NODE returning first `block_sequence_item'."
  (treesit-parent-until
   node
   (lambda (node)
     (equal (treesit-node-type node) "block_sequence_item"))))

(defun yaml-pro-ts--until-mapping-or-list (node)
  "Recursively look up from NODE returning first mapping or sequence item."
  (treesit-parent-until
   node
   (lambda (node)
     (or
      (equal (treesit-node-type node) "block_mapping_pair")
      (equal (treesit-node-type node) "block_sequence_item")))))

(defun yaml-pro-ts-kill-subtree ()
  "Kill the entire subtree located at the current-point."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (tree-top (yaml-pro-ts--until-mapping at-node)))
    (when tree-top
      (kill-region (treesit-node-start tree-top)
                   (treesit-node-end tree-top)))))

(defun yaml-pro-ts-up-level ()
  "Move the point to the parent tree."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (tree-top (yaml-pro-ts--until-mapping at-node))
         (parent-tree-top (and tree-top (yaml-pro-ts--until-mapping tree-top))))
    (when parent-tree-top
      (goto-char (treesit-node-start parent-tree-top)))))


(defun yaml-pro-ts-prev-mapping-node (tree-top type)
  "Return nearest previous sibling node of TREE-TOP of type TYPE."
  (interactive)
  (setq tree-top (treesit-node-prev-sibling tree-top))
  (while (and tree-top
              (not (equal (treesit-node-type tree-top) type)))
    (setq tree-top (treesit-node-prev-sibling tree-top)))
  tree-top)

(defun yaml-pro-ts-next-mapping-node (tree-top type)
  "Return nearest next sibling node of TREE-TOP of type TYPE."
  (interactive)
  (setq tree-top (treesit-node-next-sibling tree-top))
  (while (and tree-top
              (not (equal (treesit-node-type tree-top) type)))
    (setq tree-top (treesit-node-next-sibling tree-top)))
  tree-top)


(defun yaml-pro-ts-prev-subtree ()
  "Move the point to the previous subtree."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (tree-top (yaml-pro-ts--until-mapping at-node))
         (prev-node (yaml-pro-ts-prev-mapping-node tree-top "block_mapping_pair")))
    (when prev-node
      (goto-char (treesit-node-start prev-node)))))

(defun yaml-pro-ts-next-subtree ()
  "Move the point to the next subtree."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (tree-top (yaml-pro-ts--until-mapping at-node))
         (next-node (yaml-pro-ts-next-mapping-node tree-top "block_mapping_pair")))
    (when next-node
      (goto-char (treesit-node-start next-node)))))


(defun yaml-pro-ts-move-subtree (dir)
  "Get the current and DIR node and swap the contents of the two."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (tree-top (yaml-pro-ts--until-mapping-or-list at-node))
         (tree-top-type (treesit-node-type tree-top))
         (sibling-node (if (eq dir 'down)
                        (yaml-pro-ts-next-mapping-node tree-top tree-top-type)
                      (yaml-pro-ts-prev-mapping-node tree-top tree-top-type)))
         (at-start-marker (make-marker))
         (at-end-marker (make-marker))
         (sibling-start-marker (make-marker))
         (sibling-end-marker (make-marker))
         (sibling-end-trailing-newline nil))
    (when (and tree-top sibling-node)
      (set-marker at-start-marker (treesit-node-start tree-top))
      (set-marker at-end-marker (treesit-node-end tree-top))
      (set-marker sibling-start-marker (treesit-node-start sibling-node))
      (set-marker sibling-end-marker (treesit-node-end sibling-node))
      ;; Check if there's a trailing newline after the last node. We
      ;; do this because we don't want to insert a trailing newline if
      ;; the user doesn't have a trailing newline.
      (save-excursion
        (goto-char sibling-end-marker)
        (setq sibling-end-trailing-newline (equal (char-before) ?\n)))
      (let* ((at-tree-text (buffer-substring-no-properties
                            at-start-marker at-end-marker))
             (sibling-tree-text (buffer-substring-no-properties
                                 sibling-start-marker sibling-end-marker)))
        (delete-region at-start-marker at-end-marker)
        (delete-region sibling-start-marker sibling-end-marker)
        (goto-char sibling-start-marker)
        ;; Empty newlines that are after the last node are moved along
        ;; with the last node, so after text has been deleted from
        ;; at-node and sibling-node, there's no newline for
        ;; at-node. We only do this when the last item has a trailing
        ;; newline.
        (when (and (eobp) sibling-end-trailing-newline)
          (save-excursion
            (insert "\n")))
        (insert at-tree-text)
        (goto-char at-start-marker)
        ;; We delete the last newline (if there's one) because when we
        ;; move to `at-start-marker', there's already an empty line.
        (insert (replace-regexp-in-string "\n\\'" "" sibling-tree-text))
        (goto-char sibling-start-marker)))))

(defun yaml-pro-ts-move-subtree-up ()
  "Swap the current tree with that of the next sibling's."
  (interactive)
  (yaml-pro-ts-move-subtree 'up))

(defun yaml-pro-ts-move-subtree-down ()
  "Swap the current tree with that of the previous sibling's."
  (interactive)
  (yaml-pro-ts-move-subtree 'down))

(defun yaml-pro-ts-meta-return ()
  "Insert new list item after current item."
  (interactive)
  (let* ((at-node (treesit-node-at (point)))
         (list-item-top (yaml-pro-ts--until-list at-node))
         (list-item-end-in-newline nil))
    (when list-item-top
      (let* ((indentation (save-excursion
                            (goto-char (treesit-node-start list-item-top))
                            (current-column))))
        (goto-char (treesit-node-end list-item-top))
        ;; If the file didn't have a newline at the end of the item,
        ;; then we are looking at ^$
        (setq list-item-end-in-newline (looking-at "^$"))
        (cond
         ;; After visiting the end of the top item, we check if we are
         ;; in a newline or not. If we are not, we start a new line.
         ((not (looking-at "^$"))
          (insert "\n" (make-string indentation ?\s) "- "))
         ;; When there are empty lines above the point after going to
         ;; the end of the element. When in the top last element of a
         ;; list, treesit-node-end considers empty lines as part of
         ;; the list element.
         ((save-excursion
            (forward-line -1)
            (looking-at "^$"))
          (while (save-excursion
                   (forward-line -1)
                   (looking-at "^$"))
            (forward-line -1))
          (insert (make-string indentation ?\s) "- "))
         ;; If none of the cases are true, we are at the beginning of
         ;; a line just one line below the previous item, so we just
         ;; start another item.
         (t
          (insert (make-string indentation ?\s) "- ")))
        ;; If the the user inserted a newline at the end of the item,
        ;; we insert a newline. If the user didn't, we don't insert a
        ;; newline. We do this to conform with the POSIX definition of
        ;; a line and at the same time to act according the user's
        ;; preferences.
        (when list-item-end-in-newline
          (save-excursion
            (insert "\n")))))))

(defun yaml-pro-convolute-tree ()
  "Swap the keys of the parent of the item at point and the parent's parent."
  (interactive)
  (let* ((at-node (yaml-pro-ts--until-mapping (treesit-node-at (point))))
         (parent-tree (yaml-pro-ts--until-mapping at-node))
         (grandparent-tree (yaml-pro-ts--until-mapping
                            (treesit-node-parent parent-tree))))
    (when (and parent-tree grandparent-tree)
      (let* ((query '((block_mapping_pair key: (_) @key)))
             (parent-key (alist-get 'key (treesit-query-capture parent-tree query)))
             (parent-start (treesit-node-start parent-key))
             (parent-end (treesit-node-end parent-key))
             (parent-text (buffer-substring-no-properties parent-start parent-end))
             (grandparent-key (alist-get 'key (treesit-query-capture grandparent-tree query)))
             (grandparent-start (treesit-node-start grandparent-key))
             (grandparent-end (treesit-node-end grandparent-key))
             (grandparent-text (buffer-substring-no-properties
                                grandparent-start grandparent-end)))
        (save-excursion
          (delete-region parent-start parent-end)
          (goto-char parent-start)
          (insert grandparent-text)
          (delete-region grandparent-start grandparent-end)
          (goto-char grandparent-start)
          (insert parent-text))))))

(defun yaml-pro-ts-indent-subtree ()
  "Indent the current subtree by one level.

This function uses tree-sitter.  Indentation is controlled by the
variable `yaml-pro-indent'."
  (interactive)
  (let ((at-subtree (yaml-pro-ts--until-mapping (treesit-node-at (point))))
        (origin-marker (make-marker))
        (end-marker (make-marker))
        (indent-str (make-string yaml-pro-indent ?\s)))
    (set-marker origin-marker (point))
    (set-marker end-marker (treesit-node-end at-subtree))
    (goto-char (treesit-node-start at-subtree))
    (forward-line 0)
    (while (< (point) end-marker)
      (when (not (looking-at " *$"))
        (insert indent-str))
      (forward-line 1))
    (goto-char origin-marker)
    (set-marker origin-marker nil)
    (set-marker end-marker nil)))

(defun yaml-pro-ts-unindent-subtree ()
  "Unindent the current subtree by one level.

This function uses tree-sitter.  Indentation is controlled by the
variable `yaml-pro-indent'."
  (interactive)
  (let ((at-subtree (yaml-pro-ts--until-mapping (treesit-node-at (point))))
        (origin-marker (make-marker))
        (end-marker (make-marker))
        (indent-str (make-string yaml-pro-indent ?\s)))
    (set-marker origin-marker (point))
    (set-marker end-marker (treesit-node-end at-subtree))
    (goto-char (treesit-node-start at-subtree))
    (forward-line 0)
    (while (< (point) end-marker)
      (when (looking-at (regexp-quote indent-str))
        (delete-char yaml-pro-indent))
      (forward-line 1))
    (goto-char origin-marker)
    (set-marker origin-marker nil)
    (set-marker end-marker nil)))

(defun yaml-pro-ts-mark-subtree (up)
   "Mark the current subtree.
This puts point at the start of the current subtree, and mark at
the end.  If a numeric prefix UP is given, move up into the
hierarchy of headlines by UP levels before marking the subtree."
   (interactive "P")
   (while (and up (> up 0))
     (yaml-pro-ts-up-level)
     (cl-decf up))
   (let* ((at-subtree (yaml-pro-ts--until-mapping-or-list (treesit-node-at (point)))))
     (if (and (not up)
              (or (and (eq last-command this-command) (mark t))
                  (and transient-mark-mode mark-active)))
         (progn
           (while (and at-subtree (<= (treesit-node-end at-subtree) (mark)))
             (setq at-subtree
                   (yaml-pro-ts-next-mapping-node
                    at-subtree (treesit-node-type at-subtree))))
           (when at-subtree
             (set-mark (treesit-node-end at-subtree))))
       (push-mark (treesit-node-end at-subtree))
       (goto-char (treesit-node-start at-subtree))
       (activate-mark))))

(defun yaml-pro-ts--kill-is-subtree (&optional tree)
  "Return non-nil if TREE (or current kill) is a valid tree."
  (unless tree
    (setq tree (current-kill 0)))
  (let* ((kill (and kill-ring (current-kill 0))))
    (when (and kill (> (length (string-split kill "\n")) 1))
      (let ((node (treesit-parse-string kill 'yaml)))
        (treesit-query-capture node '((block_mapping_pair) @key))))))

(defun yaml-pro-ts-paste-subtree (&optional remove)
  "Insert the current kill into the buffer, preserving tree structure.
If REMOVE is non-nil, pop item off `kill-ring'."
  (interactive)
  (let ((tree (current-kill 0)))
    (unless (yaml-pro-ts--kill-is-subtree tree)
      (user-error
       (substitute-command-keys
        "The kill is not a YAML-tree. Use `\\[yank]' to yank anyways")))
    (let* ((base-indent (current-column))
           (indent-lengths (save-match-data
                             (seq-map
                              (lambda (line)
                                (string-match "^\\( *\\)" line)
                                (length (match-string 1 line)))
                              (seq-filter
                               (lambda (line)
                                 (string-match-p "^ *[a-zA-Z0-9\"'_-]+:" line))
                               (cdr (split-string tree "\n")))))))
      (if (not indent-lengths)
          (progn
            (push-mark (point) 'nomsg)
            (insert tree))
        (let* (;; does the killed text have more than one top level?
               ;; if so we need to calculate indentation differently.
               (single-top-lvl-p
                (seq-every-p
                 (lambda (len)
                   (<= (car indent-lengths) len))
                 (cdr indent-lengths)))
               ;; indent that the rest of kill text should have
               (kill-indent (max (+ (apply #'min indent-lengths)
                                    (if single-top-lvl-p -2 0))
                                 0))
               (kill-indent-string (make-string kill-indent ?\s))
               (base-indent-string (make-string base-indent ?\s))
               (insertion-string (with-temp-buffer
                                   (insert tree)
                                   (goto-char (point-min))
                                   (forward-line 1)
                                   (while (not (eobp))
                                     (when (looking-at kill-indent-string)
                                       (delete-char kill-indent)
                                       (insert base-indent-string))
                                     (forward-line 1))
                                   (buffer-string))))
          (push-mark (point) 'nomsg)
          (insert insertion-string)))))
  (when remove (pop kill-ring)))

(defun yaml-pro-ts-yank (&optional arg)
  "Yank text on kill ring.  If YAML-subtree, then indent it correctly.
This command will look at the current kill and check if it is a
subtree, or series of subtrees.  If so, and ARG is nil, the
subtree is yanked with the appropriate amount of whitespace
inserted to make the tree retain its original structure."
  (interactive "P")
  (setq this-command 'yank)
  (if arg
      (call-interactively #'yank)
    (let ((subtreep
           (and (yaml-pro-ts--kill-is-subtree)
                (or (bolp)
                    (and (looking-at "[ \t]*$")
                         (string-match
                          "\\` +\\'"
                          (buffer-substring (point-at-bol) (point))))))))
      (cond
       ((and subtreep yaml-pro-ts-yank-subtrees)
        (yaml-pro-ts-paste-subtree))
       (t (call-interactively #'yank))))))

(defun yaml-pro-ts--imenu-node-label (node)
  "Return an imenu label for NODE."
  (let ((top node)
        (root (treesit-buffer-root-node))
        (label ""))
    (while (not (treesit-node-eq node root))
      (cond
       ((equal (treesit-node-type node) "block_mapping_pair")
        (let ((key-node (treesit-node-child-by-field-name node "key")))
          (when key-node
            (setq label (concat (treesit-node-text key-node)
                                (if (equal label "") "" ".")
                                label)))))
       ((equal (treesit-node-type node) "block_sequence_item")
        (setq label (format "[%d].%s" (treesit-node-index node) label))))
      (setq node (treesit-node-parent node)))
    label))

(defun yaml-pro-ts-create-index ()
  "Create imenu index of YAML file using treesitter."
  (let ((all-keys (treesit-query-capture (treesit-buffer-root-node) '((block_mapping_pair key: (_) @key))))
        (imenu-items '()))
    (dolist (key-item all-keys)
      (let* ((key-node (cdr key-item))
             (key-label (yaml-pro-ts--imenu-node-label key-node)))
        (push (cons key-label (treesit-node-start key-node)) imenu-items)))
    (seq-reverse imenu-items)))

(defun yaml-pro-ts-eldoc (&rest _ignored)
  "Return eldoc message of current point."
  (let* ((at-tree (yaml-pro-ts--until-mapping-or-list (treesit-node-at (point)))))
    (when at-tree
      (yaml-pro-ts--imenu-node-label at-tree))))

(defconst yaml-pro-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map (kbd "C-c C-x C-w") #'yaml-pro-ts-kill-subtree)
      (define-key map (kbd "C-c C-u") #'yaml-pro-ts-up-level)

      (define-key map (kbd "C-c C-p") #'yaml-pro-ts-prev-subtree)
      (define-key map (kbd "C-c C-n") #'yaml-pro-ts-next-subtree)

      (define-key map (kbd "s-<up>") #'yaml-pro-ts-move-subtree-up)
      (define-key map (kbd "s-<down>") #'yaml-pro-ts-move-subtree-down)

      (define-key map (kbd "C-c >") #'yaml-pro-ts-indent-subtree)
      (define-key map (kbd "C-c <") #'yaml-pro-ts-unindent-subtree)

      (define-key map (kbd "M-<return>") #'yaml-pro-ts-meta-return)
      (define-key map (kbd "M-?") #'yaml-pro-convolute-tree)
      (define-key map (kbd "C-c @") #'yaml-pro-ts-mark-subtree)
      (define-key map (kbd "C-c C-x C-y") #'yaml-pro-ts-paste-subtree)

      (define-key map [remap yank] #'yaml-pro-ts-yank)

      (define-key map (kbd "C-c '") #'yaml-pro-edit-ts-scalar)))
  "Map for minor mode `yaml-pro-ts-mode'.")

(define-minor-mode yaml-pro-ts-mode
  "Minor mode to enable yaml-pro treesitter keymap.

\\{yaml-pro-mode-map}"
  :init-value nil
  :group 'yaml-pro
  :keymap yaml-pro-ts-mode-map

  (when yaml-pro-ts-mode
    (unless (featurep 'treesit)
      (user-error "Tree-sitter not supported in current Emacs version"))
    (unless (treesit-ready-p 'yaml)
      (user-error "YAML tree-sitter not ready"))
    (treesit-parser-create 'yaml)
    (setq imenu-generic-expression nil)
    (setq imenu-create-index-function #'yaml-pro-ts-create-index)
    (if (boundp 'eldoc-documentation-functions)
        (add-hook 'eldoc-documentation-functions #'yaml-pro-ts-eldoc nil t)
      (setq-local eldoc-documentation-function #'yaml-pro-ts-eldoc))))


;;; yaml-pro parser

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
   (t (concat (string-join path " ") ": " tree))))

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
          (error "Subtree can't be unintented further")))
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

(defun yaml-pro-create-index ()
  "Create an imenu index using the legacy parser."
  (let* ((tree (yaml-parse-string-with-pos (buffer-string)))
         (paths (yaml-pro--extract-paths tree))
         (sorted-paths (seq-sort-by (lambda (path)
                                      (car (yaml-pro--get-last-yaml-pos path)))
                                    #'< paths)))
    (seq-map
     (lambda (item)
       (let ((pos (car (yaml-pro--get-last-yaml-pos item))))
         (cons item pos)))
     sorted-paths)))

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

(make-obsolete 'yaml-pro-consult-jump "Use imenu feature instead of this command." "0.3.2")

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
        (setq imenu-generic-expression nil)
        (setq imenu-create-index-function #'yaml-pro-create-index)
        (when (equal mode-name "YAML")
          (add-hook 'after-change-functions #'yaml-pro--after-change-hook nil t)))
    (remove-hook 'after-change-functions #'yaml-pro--after-change-hook t)))

(provide 'yaml-pro)

;;; yaml-pro.el ends here
