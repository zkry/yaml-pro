(defmacro yaml-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with yaml-mode as the active
mode holding TEXT.

This macro is based on the function org-test-with-temp-text from
org-test.el, the library that is used by Org Mode for writing tests"
  (declare (indent 1) (debug t))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	 (yaml-mode-hook nil))
     (with-temp-buffer
       (yaml-mode)
       (yaml-pro-ts-mode)
       (let ((point (string-match "<point>" inside-text)))
	 (if point
	     (progn
	       (insert (replace-match "" nil nil inside-text))
	       (goto-char (1+ (match-beginning 0))))
	   (insert inside-text)
	   (goto-char (point-min))))
       (font-lock-ensure (point-min) (point-max))
       ,@body)))

(ert-deftest test-yaml-pro-ts-next-subtree ()
  :tags '(yaml-pro-ts-next-subtree)
  (yaml-test-with-temp-text (string-join
                             '("1:"
                               "  1.1: null"
                               "<point>2:"
                               "  2.1: null"
                               "3:"
                               "  3.1: null")
                             "\n")
    (yaml-pro-ts-next-subtree)
    (should
     (equal
      (thing-at-point 'line t)
      "3:\n"))))

(ert-deftest test-yaml-pro-ts-prev-subtree ()
  :tags '(yaml-pro-ts-prev-subtree)
  (yaml-test-with-temp-text (string-join
                             '("1:"
                               "  1.1: null"
                               "<point>2:"
                               "  2.1: null"
                               "3:"
                               "  3.1: null")
                             "\n")
    (yaml-pro-ts-prev-subtree)
    (should
     (equal
      (thing-at-point 'line t)
      "1:\n"))))

(ert-deftest test-yaml-pro-ts-move-subtree-up ()
  :tags '(yaml-pro-ts-move-subtree-up)
  (yaml-test-with-temp-text (string-join
                             '("1:"
                               "  1.1: null"
                               "<point>2:"
                               "  2.1: null"
                               "3:"
                               "  3.1: null"
                               "\n")
                             "\n")
    (yaml-pro-ts-move-subtree-up)
    (should
     (equal
      (string-join
       '("2:"
         "  2.1: null"
         "1:"
         "  1.1: null"
         "3:"
         "  3.1: null"
         "\n")
       "\n")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-yaml-pro-ts-move-subtree-down ()
  :tags '(yaml-pro-ts-move-subtree-down)
  (yaml-test-with-temp-text (string-join
                             '("<point>1:"
                               "  1.1: null"
                               "2:"
                               "  2.1: null"
                               "3:"
                               "  3.1: null")
                             "\n")
    (yaml-pro-ts-move-subtree-down)
    (should
     (equal
      (string-join
       '("2:"
         "  2.1: null"
         "1:"
         "  1.1: null"
         "3:"
         "  3.1: null")
       "\n")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-yaml-pro-ts-unindent-subtree ()
  :tags '(yaml-pro-ts-unindent-subtree)
  (yaml-test-with-temp-text (string-join
                             '("- 1: null"
                               "  2: null"
                               "  3:"
                               "  - <point>3.1:")
                             "\n")
    (yaml-pro-ts-unindent-subtree)
    (should
     (equal
      (string-join
       '("- 1: null"
         "  2: null"
         "  3:"
         "- 3.1:")
       "\n")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-yaml-pro-ts-indent-subtree ()
  :tags '(yaml-pro-ts-indent-subtree)
  (yaml-test-with-temp-text (string-join
                             '("- 1.1: null"
                               "  1.2: null"
                               "  1.3: "
                               "- 2.1: <point>")
                             "\n")
    (yaml-pro-ts-indent-subtree)
    (should
     (equal
      (string-join
       '("- 1.1: null"
         "  1.2: null"
         "  1.3: "
         "  - 2.1: ")
       "\n")
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-yaml-pro-ts-meta-return ()
  :tags '(yaml-pro-ts-meta-return)
  (yaml-test-with-temp-text "- 1: one<point>"
    (yaml-pro-ts-meta-return)
    (should
     (equal
      "- 1: one\n- "
      (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-yaml-pro-ts-mark-subtree ()
  :tags '(yaml-pro-ts-mark-subtree)
  (yaml-test-with-temp-text (string-join
                             '("1:"
                               "  1.1: null"
                               "<point>2:"
                               "  2.1:"
                               "    2.1.1: null"
                               "3:"
                               "  3.1: null")
                             "\n")
    ;; If we don't inhibit messages, the message "Mark set" is
    ;; shown. We don't want that message to be shown when running
    ;; tests since that clutters the output
    (let ((inhibit-message t))
      (call-interactively 'yaml-pro-ts-mark-subtree))
    (should
     (equal
      (buffer-substring-no-properties
       (region-beginning)
       (region-end))
      (string-join
       '("2:"
         "  2.1:"
         "    2.1.1: null")
       "\n")))))
