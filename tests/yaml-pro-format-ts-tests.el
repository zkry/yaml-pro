;;; yaml-pro-format-ts-tests.el --- Tests for formatting -*- lexical-binding: t -*-

(defun yaml-pro-format-ts-tests-extract-prettier-tests (prettier/tests/format/yaml-directory)
  "Function to extract all Jest snapshot tests from a given directory.
PRETTIER/TESTS/FORMAT/YAML-DIRECTORY is the file path to where the tests are located."
  (let* ((files (directory-files-recursively prettier/tests/format/yaml-directory "jsfmt.spec.js.snap"))
         (out-buf (generate-new-buffer "*prettier.js-test-gen*"))
         (cases '()))
    (dolist (file files)
      (message "FILE: %s" file)
      (with-current-buffer (find-file-noselect file)
        (js-ts-mode)
        (let* ((capture (treesit-query-capture (treesit-buffer-root-node)
                                               '((expression_statement
                                                  (assignment_expression
                                                   left: (subscript_expression (template_string))
                                                   "="
                                                   right: (template_string)))
                                                 @node ))))
          (pcase-dolist (`(_ . ,node) capture)
            (let* ((children (treesit-query-capture node
                                                    '((expression_statement
                                                       (assignment_expression
                                                        left: (subscript_expression (template_string) @key)
                                                        "="
                                                        right: (template_string) @value)))))
                   (key-node (alist-get 'key children))
                   (value-node (alist-get 'value children))
                   (_key (string-trim (treesit-node-text key-node) "`" "`"))
                   (value (string-trim (treesit-node-text value-node) "`" "`"))
                   (value-parts (string-split value "^=+[a-z]*=+$" t "[ \t\n]*"))
                   (opts (yaml-parse-string (replace-regexp-in-string "^ +| printWidth" "" (car value-parts)) :object-type 'alist)))
              (push (cons opts (cdr (seq-map (lambda (pt)
                                               (substring-no-properties pt 0 (length pt)))
                                             value-parts)))
                    cases))))))
    (with-current-buffer out-buf
      (insert (prin1-to-string cases)))
    (display-buffer out-buf)))

(defun yaml-pro-format-ts-tests--run-test (test-case &optional display-diff)
  (pcase-let* ((`(,options ,in ,out) test-case))
    (let* ((yaml-pro-indent (or (alist-get 'tabWidth options) 2))
           (got (with-temp-buffer
                  (insert in)
                  (yaml-ts-mode)
                  (benchmark-call #'yaml-pro-format-ts)
                  (buffer-string))))
      (when (and display-diff (not (equal (string-trim got) (string-trim out))))
        (let* ((ba (generate-new-buffer "*a*"))
               (bb (generate-new-buffer "*b*")))
          (with-current-buffer ba
            (insert (string-trim got)))
          (with-current-buffer bb
            (insert (string-trim out)))
          (diff-buffers ba bb)))
      (equal (string-trim got) (string-trim out)))))

(defvar yaml-pro-format-fail-cases nil)


;; (yaml-pro-format-ts-tests--run)
(defun yaml-pro-format-ts-tests--run ()
  "Run test suite, writing failed tests to file."
  (setq yaml-pro-format-fail-cases nil)
  (let* ((total (length yaml-pro-format-ts-tests-cases))
         (ct 0)
         (i 0))
    (dolist (test-case yaml-pro-format-ts-tests-cases)
      (message "TEST: %d" i)
      (let* ((result (ignore-errors (yaml-pro-format-ts-tests--run-test test-case))))
        (if result
            (cl-incf ct)
          (push test-case yaml-pro-format-fail-cases)))
      (cl-incf i))
    (message "Ran all tests! Results: %d/%d" ct total)
    (sit-for 2))
  (f-write (prin1-to-string yaml-pro-format-fail-cases) 'utf-8 "./fail-cases.el"))

(ert-deftest yaml-pro-format-ts-tests ()
  "Test the yaml-pro-format-ts command."
  (dolist (test-case yaml-pro-format-ts-tests-cases)
    (should (yaml-pro-format-ts-tests--run-test test-case))))

;; copied via the function `yaml-pro-format-ts-tests-extract-prettier-tests'
(defconst yaml-pro-format-ts-tests-cases
  '((((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "- &abc a
- *abc" "- &abc a
- *abc") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "---
- hosts: webservers
  vars:
    http_port: 80
    max_clients: 200
  remote_user: root
  tasks:
  - name: ensure apache is at the latest version
    yum:
      name: httpd
      state: latest
  - name: write the apache config file
    template:
      src: /srv/httpd.j2
      dest: /etc/httpd.conf
    notify:
    - restart apache
  - name: ensure apache is running
    service:
      name: httpd
      state: started
  handlers:
    - name: restart apache
      service:
        name: httpd
        state: restarted" "---
- hosts: webservers
  vars:
    http_port: 80
    max_clients: 200
  remote_user: root
  tasks:
    - name: ensure apache is at the latest version
      yum:
        name: httpd
        state: latest
    - name: write the apache config file
      template:
        src: /srv/httpd.j2
        dest: /etc/httpd.conf
      notify:
        - restart apache
    - name: ensure apache is running
      service:
        name: httpd
        state: started
  handlers:
    - name: restart apache
      service:
        name: httpd
        state: restarted") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) ">
    123
    456
    789" ">
  123
  456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) ">2-
    123
   456
  789" ">2-
    123
   456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) ">2-
    123
   456
  789" ">2-
    123
   456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) ">+
    123
    456
    789" ">+
  123
  456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: >
  123
  456
  789
b: >1
    123
   456
  789
d: >
  123
  456
  789

c: 0" "a: >
  123
  456
  789
b: >1
    123
   456
  789
d: >
  123
  456
  789

c: 0") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "!!str #comment
>
  123" "!!str #comment
>
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!str #comment
>
  123" "!!str #comment
>
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "- >+
  123
  456
  789

  123
  456
  789


  123
  456
  789


- >2+
  123
  456
  789

  123
  456
  789


  123
  456
  789

- 0" "- >+
  123
  456
  789

  123
  456
  789


  123
  456
  789


- >2+
  123
  456
  789

  123
  456
  789


  123
  456
  789

- 0") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) ">
  1
  2
    3
    4
  5
  6

  1
  2
    3
    4
  5
  6


  1
  2
    3
    4
  5
  6

  1
  2

    3
    4

  5
  6" ">
  1
  2
    3
    4
  5
  6

  1
  2
    3
    4
  5
  6


  1
  2
    3
    4
  5
  6

  1
  2

    3
    4

  5
  6") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "!!str &anchor >
  123" "!!str &anchor >
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!str &anchor >
  123" "!!str &anchor >
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a: !!str &anchor >
  123" "a: !!str &anchor >
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: !!str &anchor >
  123" "a: !!str &anchor >
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) ">
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
---
>
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789" ">
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
  123 456 789
---
>
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789
  123   456   789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "- >
  123
  456
  789
- >1
    123
   456
  789
- 0" "- >
  123
  456
  789
- >1
    123
   456
  789
- 0") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) ">-
    123
    456
    789" ">-
  123
  456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a: !!str > # hello
  hello" "a: !!str > # hello
  hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: !!str > # hello
  hello" "a: !!str > # hello
  hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "|
    123
    456
    789" "|
  123
  456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "|
    123
    456
    789" "|
  123
  456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "|2-
    123
   456
  789" "|2-
    123
   456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "|2-
    123
   456
  789" "|2-
    123
   456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "|+
    123
    456
    789" "|+
  123
  456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "|+
    123
    456
    789" "|+
  123
  456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a: |
  123
  456
  789
b: |1
    123
   456
  789
d: |
  123
  456
  789

c: 0" "a: |
  123
  456
  789
b: |1
    123
   456
  789
d: |
  123
  456
  789

c: 0") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: |
  123
  456
  789
b: |1
    123
   456
  789
d: |
  123
  456
  789

c: 0" "a: |
  123
  456
  789
b: |1
    123
   456
  789
d: |
  123
  456
  789

c: 0") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "!!str #comment
|
  123" "!!str #comment
|
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!str #comment
|
  123" "!!str #comment
|
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- |+
  123
  456
  789



- 0" "- |+
  123
  456
  789



- 0") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "- |+
  123
  456
  789



- 0" "- |+
  123
  456
  789



- 0") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "|
  1
  2
    3
    4
  5
  6

  1
  2
    3
    4
  5
  6


  1
  2
    3
    4
  5
  6

  1
  2

    3
    4

  5
  6" "|
  1
  2
    3
    4
  5
  6

  1
  2
    3
    4
  5
  6


  1
  2
    3
    4
  5
  6

  1
  2

    3
    4

  5
  6") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "|
  1
  2
    3
    4
  5
  6

  1
  2
    3
    4
  5
  6


  1
  2
    3
    4
  5
  6

  1
  2

    3
    4

  5
  6" "|
  1
  2
    3
    4
  5
  6

  1
  2
    3
    4
  5
  6


  1
  2
    3
    4
  5
  6

  1
  2

    3
    4

  5
  6") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "!!str &anchor |
  123" "!!str &anchor |
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!str &anchor |
  123" "!!str &anchor |
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a: !!str &anchor |
  123" "a: !!str &anchor |
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: !!str &anchor |
  123" "a: !!str &anchor |
  123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- |
  123
  456
  789
- |1
    123
   456
  789
- 0" "- |
  123
  456
  789
- |1
    123
   456
  789
- 0") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "- |
  123
  456
  789
- |1
    123
   456
  789
- 0" "- |
  123
  456
  789
- |1
    123
   456
  789
- 0") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "|-
    123
    456
    789" "|-
  123
  456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "|-
    123
    456
    789" "|-
  123
  456
  789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a: !!str | # hello
  hello" "a: !!str | # hello
  hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: !!str | # hello
  hello" "a: !!str | # hello
  hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "# --- comments ---" "# --- comments ---") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "- foo: 0
  bar: 1

  # baz: 2
- quux: 3

- foo: 0
  bar: 1

  # baz: 2

  # baz: 3
- quux: 3" "- foo: 0
  bar: 1

  # baz: 2
- quux: 3

- foo: 0
  bar: 1

  # baz: 2

  # baz: 3
- quux: 3") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "foo1:
  - foo

  # - foo

  # - foo

  - foo

foo2:
  - foo2

  # - foo2






  # - foo2
  # - foo2" "foo1:
  - foo

  # - foo

  # - foo

  - foo

foo2:
  - foo2

  # - foo2

  # - foo2
  # - foo2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "foo1:
  - foo

  # - foo

  # - foo

  - foo

foo2:
  - foo2

  # first line
  # next line" "foo1:
  - foo

  # - foo

  # - foo

  - foo

foo2:
  - foo2

  # first line
  # next line") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "#6445

obj:
  # before


  # before


  key: value


  # after


  # after" "#6445

obj:
  # before

  # before

  key: value

  # after

  # after") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "#hello world" "#hello world") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "-  - a

   # - b

   # - c

   - e

-  - a

   # - b

   # - c" "- - a

  # - b

  # - c

  - e

- - a

  # - b

  # - c") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "%YAML 1.2
---" "%YAML 1.2
---") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "%SOMETHING
---" "%SOMETHING
---") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "# 123
%YAML 1.2
# 456
---
# 789
test
# 000" "# 123
%YAML 1.2
# 456
---
# 789
test
# 000") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "---
123" "---
123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "---666
123" "---666
123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "123" "123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "{&123 foo, *123 : 456}" "{ &123 foo, *123 : 456 }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{&123 foo, *123 : 456}" "{ &123 foo, *123 : 456 }") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{[longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong],[longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong],[longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong]}" "{
  [
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
  ],
  [
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
  ],
  [
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
  ],
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "{[longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong],[longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong],[longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong]}" "{
    [
        longlonglonglonglonglonglonglonglonglonglong,
        longlonglonglonglonglonglonglonglonglonglong,
        longlonglonglonglonglonglonglonglonglonglong,
    ],
    [
        longlonglonglonglonglonglonglonglonglonglong,
        longlonglonglonglonglonglonglonglonglonglong,
        longlonglonglonglonglonglonglonglonglonglong,
    ],
    [
        longlonglonglonglonglonglonglonglonglonglong,
        longlonglonglonglonglonglonglonglonglonglong,
        longlonglonglonglonglonglonglonglonglonglong,
    ],
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{[longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong],[longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong],[longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong, longlonglonglonglonglonglonglonglonglonglong]}" "{
  [
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
  ],
  [
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
  ],
  [
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglong,
  ],
}") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{123, # comment
}" "{
  123, # comment
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "{123, # comment
}" "{
    123, # comment
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{123, # comment
}" "{
  123, # comment
}") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{}" "{}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "{}" "{}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{}" "{}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "{ : }" "{ : }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{ : }" "{ : }") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong}" "{
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "{longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong}" "{
    longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
    longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong}" "{
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
}") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3}" "{
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1,
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2,
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3,
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "{longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3}" "{
    longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1,
    longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2,
    longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3,
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2,longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3}" "{
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong1,
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong2,
  longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong3,
}") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong}" "{
  1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
  2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
  3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "{1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong}" "{
    1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
    2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
    3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong}" "{
  1: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
  2: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
  3: longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglong,
}") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!map #comment
{}" "!!map #comment
{}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "!!map #comment
{}" "!!map #comment
{}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!map #comment
{}" "!!map #comment
{}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "!!map &anchor {a: 1}" "!!map &anchor { a: 1 }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!map &anchor {a: 1}" "!!map &anchor { a: 1 }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "a: !!map &anchor {a: 1}" "a: !!map &anchor { a: 1 }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: !!map &anchor {a: 1}" "a: !!map &anchor { a: 1 }") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{
x: 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
}" "{
  x: 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890,
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "{
x: 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
}" "{
    x: 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890,
}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "{
x: 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
}" "{
  x: 12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890,
}") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "[&123 foo, *123 : 456]" "[&123 foo, *123 : 456]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "[&123 foo, *123 : 456]" "[&123 foo, *123 : 456]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "[&123 foo, *123 : 456]" "[&123 foo, *123 : 456]") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "[123, # comment
]" "[
  123, # comment
]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "[123, # comment
]" "[
    123, # comment
]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "[123, # comment
]" "[
  123, # comment
]") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "[]" "[]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "[]" "[]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "[]" "[]") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!set # comment
[]" "!!set # comment
[]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "!!set # comment
[]" "!!set # comment
[]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!set # comment
[]" "!!set # comment
[]") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!set &anchor [1]" "!!set &anchor [1]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "!!set &anchor [1]" "!!set &anchor [1]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!set &anchor [1]" "!!set &anchor [1]") (((bracketSpacing . :false) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: !!set &anchor [1]" "a: !!set &anchor [1]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "a: !!set &anchor [1]" "a: !!set &anchor [1]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: !!set &anchor [1]" "a: !!set &anchor [1]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "# 8876

foo:
 <<: &anchor
   K1: \"One\"
 K2: \"Two\"

bar:
 <<: *anchor
 K3: \"Three\"" "# 8876

foo:
  <<: &anchor
    K1: \"One\"
  K2: \"Two\"

bar:
  <<: *anchor
  K3: \"Three\"") (((insertPragma . t) (parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "# @prettier

    123" "# @prettier

123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "[1, 2, 3]: 123" "[1, 2, 3]: 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "[1, 2, 3]: 123" "[1, 2, 3]: 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "123: [1, 2, 3]" "123: [1, 2, 3]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "123: [1, 2, 3]" "123: [1, 2, 3]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "123: # hello" "123: # hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "123: # hello" "123: # hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "- a: b
  c: d" "- a: b
  c: d") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "- a: b
  c: d" "- a: b
  c: d") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "key:
  key: value" "key:
    key: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "key:
  key: value" "key:
  key: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) ".anchors:
  - &anchor1
    key: value
  - &anchor2
    another: prop

foo:
  bar: baz
  <<: *anchor1
  <<: *anchor2" ".anchors:
  - &anchor1
    key: value
  - &anchor2
    another: prop

foo:
  bar: baz
  <<: *anchor1
  <<: *anchor2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "!!map # comment
a: 123" "!!map # comment
a: 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!map # comment
a: 123" "!!map # comment
a: 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "--- !!map &anchor
a: 123" "---
!!map &anchor
a: 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "--- !!map &anchor
a: 123" "---
!!map &anchor
a: 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "a: !!map &anchor
  a: 123" "a: !!map &anchor
    a: 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: !!map &anchor
  a: 123" "a: !!map &anchor
  a: 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "key:
- value" "key:
    - value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "key:
- value" "key:
  - value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "!!str # comment
hello" "!!str # comment
hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("never" 0 5 (fontified nil)))) "!!str # comment
hello" "!!str # comment
hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!str # comment
hello" "!!str # comment
hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "b:
  123123123123123123123123123
       123123123123123123123123123
         123123123123123123123123123
    123123123123123123123123123
      123123123123123123123123123
  123123123123123123123123123
            123123123123123123123123123

         123123123123123123123123123


         123123123123123123123123123




         123123123123123123123123123" "b: 123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123

  123123123123123123123123123


  123123123123123123123123123




  123123123123123123123123123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "hello" "hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("never" 0 5 (fontified nil)))) "hello" "hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "hello" "hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "!<hello> hello" "!<hello> hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("never" 0 5 (fontified nil)))) "!<hello> hello" "!<hello> hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!<hello> hello" "!<hello> hello") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: \"
  123123123123123123123123123
       123123123123123123123123123
         123123123123123123123123123
    123123123123123123123123123
      123123123123123123123123123
  123123123123123123123123123
            123123123123123123123123123

         123123123123123123123123123


         123123123123123123123123123




         123123123123123123123123123
    \"
b: '
  123123123123123123123123123
       123123123123123123123123123
         123123123123123123123123123
    123123123123123123123123123
      123123123123123123123123123
  123123123123123123123123123
            123123123123123123123123123

         123123123123123123123123123


         123123123123123123123123123




         123123123123123123123123123
    '" "a: \"
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123

  123123123123123123123123123


  123123123123123123123123123




  123123123123123123123123123
  \"
b: \"
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123
  123123123123123123123123123

  123123123123123123123123123


  123123123123123123123123123




  123123123123123123123123123
  \"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (requirePragma . t)) "# @prettier

    123" "# @prettier

123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (requirePragma . t)) "123" "123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "---
123
...
---
456" "---
123
---
456") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "- 123
- 456
- 789" "- 123
- 456
- 789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "- 123
- 456
- 789" "- 123
- 456
- 789") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "!!set # comment
- 123" "!!set # comment
- 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "!!set # comment
- 123" "!!set # comment
- 123") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "- 1
- - 2-1
  - 2-2
- - - 3-1-1
    - 3-2-1" "- 1
- - 2-1
  - 2-2
- - - 3-1-1
    - 3-2-1") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "- 1
- - 2-1
  - 2-2
- - - 3-1-1
    - 3-2-1" "- 1
- - 2-1
  - 2-2
- - - 3-1-1
    - 3-2-1") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "-" "-") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "-" "-") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "--- !!set &anchor
- 123
- 456" "---
!!set &anchor
- 123
- 456") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "--- !!set &anchor
- 123
- 456" "---
!!set &anchor
- 123
- 456") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (tabWidth . 4)) "a: !!set &anchor
- 123
- 456" "a: !!set &anchor
    - 123
    - 456") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80)) "a: !!set &anchor
- 123
- 456" "a: !!set &anchor
  - 123
  - 456") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- &a a
- &b b
- *a
- *b" "- &a a
- &b b
- *a
- *b") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- &a a
- &b b
- *a
- *b" "- &a a
- &b b
- *a
- *b") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a: &:@*!$\"<foo>: scalar a
b: *:@*!$\"<foo>:" "a: &:@*!$\"<foo>: scalar a
b: *:@*!$\"<foo>:") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "a: &:@*!$\"<foo>: scalar a
b: *:@*!$\"<foo>:" "a: &:@*!$\"<foo>: scalar a
b: *:@*!$\"<foo>:") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a!\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_\\`az{|}~: safe
?foo: safe question mark
:foo: safe colon
-foo: safe dash
this is#not: a comment" "a!\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_\\`az{|}~: safe
?foo: safe question mark
:foo: safe colon
-foo: safe dash
this is#not: a comment") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "a!\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_\\`az{|}~: safe
?foo: safe question mark
:foo: safe colon
-foo: safe dash
this is#not: a comment" "a!\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_\\`az{|}~: safe
?foo: safe question mark
:foo: safe colon
-foo: safe dash
this is#not: a comment") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "safe: a!\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_\\`az{|}~
     !\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_\\`az{|}~
safe question mark: ?foo
safe colon: :foo
safe dash: -foo" "safe: a!\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_\\`az{|}~
  !\"#$%&'()*+,-./09:;<=>?@AZ[\\\\]^_\\`az{|}~
safe question mark: ?foo
safe colon: :foo
safe dash: -foo") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "\"foo\\\\nbar:baz\\\\tx \\\\\\\\$%^&*()x\": 23
'x\\\\ny:z\\\\tx $%^&*()x': 24" "\"foo\\\\nbar:baz\\\\tx \\\\\\\\$%^&*()x\": 23
'x\\\\ny:z\\\\tx $%^&*()x': 24") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "\"foo\\\\nbar:baz\\\\tx \\\\\\\\$%^&*()x\": 23
'x\\\\ny:z\\\\tx $%^&*()x': 24" "\"foo\\\\nbar:baz\\\\tx \\\\\\\\$%^&*()x\": 23
'x\\\\ny:z\\\\tx $%^&*()x': 24") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
- &😁 unicode anchor" "---
- &😁 unicode anchor") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
- &😁 unicode anchor" "---
- &😁 unicode anchor") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "&a a: b
c: &d d" "&a a: b
c: &d d") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "&a a: b
c: &d d" "&a a: b
c: &d d") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "&a: key: &a value
foo:
  *a:" "&a: key: &a value
foo: *a:") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "&a: key: &a value
foo:
  *a:" "&a: key: &a value
foo: *a:") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "'foo: bar\\\\': baz'" "'foo: bar\\\\': baz'") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "'foo: bar\\\\': baz'" "'foo: bar\\\\': baz'") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
scalar1
...
key: value" "---
scalar1
---
key: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
scalar1
...
key: value" "---
scalar1
---
key: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "foo: 1

bar: 2

text: |
  a

  b

  c

  d" "foo: 1

bar: 2

text: |
  a

  b

  c

  d") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "foo: 1

bar: 2

text: |
  a

  b

  c

  d" "foo: 1

bar: 2

text: |
  a

  b

  c

  d") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "key:
 - item1
 - item2" "key:
  - item1
  - item2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "key:
 - item1
 - item2" "key:
  - item1
  - item2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- - s1_i1
  - s1_i2
- s2" "- - s1_i1
  - s1_i2
- s2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- - s1_i1
  - s1_i2
- s2" "- - s1_i1
  - s1_i2
- s2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "foo:
  bar: 1
baz: 2" "foo:
  bar: 1
baz: 2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "foo:
  bar: 1
baz: 2" "foo:
  bar: 1
baz: 2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "canonical: !!binary \"\\\\
 R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5\\\\
 OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+\\\\
 +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC\\\\
 AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=\"
generic: !!binary |
 R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
 OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
 +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
 AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=
description:
 The binary value above is a tiny arrow encoded as a gif image." "canonical: !!binary \"\\\\
  R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5\\\\
  OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+\\\\
  +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC\\\\
  AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=\"
generic: !!binary |
  R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
  OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
  +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
  AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=
description: The binary value above is a tiny arrow encoded as a gif image.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "canonical: !!binary \"\\\\
 R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5\\\\
 OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+\\\\
 +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC\\\\
 AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=\"
generic: !!binary |
 R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
 OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
 +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
 AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=
description:
 The binary value above is a tiny arrow encoded as a gif image." "canonical: !!binary \"\\\\
  R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5\\\\
  OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+\\\\
  +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC\\\\
  AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=\"
generic: !!binary |
  R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
  OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
  +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
  AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=
description: The binary value above is a tiny arrow encoded as a gif image.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
a: b
---" "---
a: b
---") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
a: b
---" "---
a: b
---") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) ":" ":") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) ":" ":") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "1: 2


3: 4" "1: 2

3: 4") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "1: 2


3: 4" "1: 2

3: 4") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "escaped slash: \"a\\\\/b\"" "escaped slash: \"a\\\\/b\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "escaped slash: \"a\\\\/b\"" "escaped slash: \"a\\\\/b\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "! a" "! a") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "! a" "! a") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "{foo: you, bar: far}" "{ foo: you, bar: far }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "{foo: you, bar: far}" "{ foo: you, bar: far }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- {a: b}" "- { a: b }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- {a: b}" "- { a: b }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "[foo, bar, 42]" "[foo, bar, 42]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "[foo, bar, 42]" "[foo, bar, 42]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a: [b, c]" "a: [b, c]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "a: [b, c]" "a: [b, c]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "{a: [b, c], [d, e]: f}" "{ a: [b, c], [d, e]: f }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "{a: [b, c], [d, e]: f}" "{ a: [b, c], [d, e]: f }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "[a, [b, c]]" "[a, [b, c]]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "[a, [b, c]]" "[a, [b, c]]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) ">
 ab
 cd

 ef


 gh" ">
  ab
  cd

  ef


  gh") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "[flow]: block" "[flow]: block") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "[flow]: block" "[flow]: block") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
wanted: love ♥ and peace ☮" "---
wanted: love ♥ and peace ☮") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
wanted: love ♥ and peace ☮" "---
wanted: love ♥ and peace ☮") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- bla\"keks: foo
- bla]keks: foo" "- bla\"keks: foo
- bla]keks: foo") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- bla\"keks: foo
- bla]keks: foo" "- bla\"keks: foo
- bla]keks: foo") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
&mapping
&key [ &item a, b, c ]: value" "---
&mapping
&key [&item a, b, c]: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
&mapping
&key [ &item a, b, c ]: value" "---
&mapping
&key [&item a, b, c]: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a:
  b:
    c: d
  e:
    f: g
h: i" "a:
  b:
    c: d
  e:
    f: g
h: i") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "a:
  b:
    c: d
  e:
    f: g
h: i" "a:
  b:
    c: d
  e:
    f: g
h: i") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
plain: a
 b

 c" "---
plain: a
  b

  c") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "a
b
  c
d

e" "a
b
c
d

e") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "a: b
 c
d:
 e
  f" "a: b
  c
d: e
  f") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
scalar
%YAML 1.2" "---
scalar
%YAML 1.2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- foo
- bar
- 42" "- foo
- bar
- 42") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- foo
- bar
- 42" "- foo
- bar
- 42") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "foo: blue
bar: arrr
baz: jazz" "foo: blue
bar: arrr
baz: jazz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "foo: blue
bar: arrr
baz: jazz" "foo: blue
bar: arrr
baz: jazz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- plain
- \"double quoted\"
- 'single quoted'
- >
  block
- plain again" "- plain
- \"double quoted\"
- \"single quoted\"
- >
  block
- plain again") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- plain
- \"double quoted\"
- 'single quoted'
- >
  block
- plain again" "- plain
- \"double quoted\"
- \"single quoted\"
- >
  block
- plain again") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
key ends with two colons::: value" "---
key ends with two colons::: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
key ends with two colons::: value" "---
key ends with two colons::: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "k:#foo
 &a !t s" "k:#foo
&a !t s") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
plain\\\\value\\\\with\\\\backslashes" "---
plain\\\\value\\\\with\\\\backslashes") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
plain\\\\value\\\\with\\\\backslashes" "---
plain\\\\value\\\\with\\\\backslashes") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- { url: http://example.org }" "- { url: http://example.org }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- { url: http://example.org }" "- { url: http://example.org }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "--- \"quoted
string\"
--- &node foo" "---
\"quoted
string\"
---
&node foo") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- single multiline
 - sequence entry" "- single multiline
  - sequence entry") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "foo:
- 42
bar:
  - 44" "foo:
  - 42
bar:
  - 44") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "foo:
- 42
bar:
  - 44" "foo:
  - 42
bar:
  - 44") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "1:
- 2
- 3
4: 5" "1:
  - 2
  - 3
4: 5") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "1:
- 2
- 3
4: 5" "1:
  - 2
  - 3
4: 5") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "foo:
  bar: baz" "foo:
  bar: baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "foo:
  bar: baz" "foo:
  bar: baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- foo" "- foo") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- foo" "- foo") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "foo: bar" "foo: bar") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "foo: bar" "foo: bar") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- Mark McGwire
- Sammy Sosa
- Ken Griffey" "- Mark McGwire
- Sammy Sosa
- Ken Griffey") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- Mark McGwire
- Sammy Sosa
- Ken Griffey" "- Mark McGwire
- Sammy Sosa
- Ken Griffey") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In" "hr: 65 # Home runs
avg: 0.278 # Batting average
rbi: 147 # Runs Batted In") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In" "hr: 65 # Home runs
avg: 0.278 # Batting average
rbi: 147 # Runs Batted In") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves" "american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves" "american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "-
  name: Mark McGwire
  hr:   65
  avg:  0.278
-
  name: Sammy Sosa
  hr:   63
  avg:  0.288" "- name: Mark McGwire
  hr: 65
  avg: 0.278
- name: Sammy Sosa
  hr: 63
  avg: 0.288") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "-
  name: Mark McGwire
  hr:   65
  avg:  0.278
-
  name: Sammy Sosa
  hr:   63
  avg:  0.288" "- name: Mark McGwire
  hr: 65
  avg: 0.278
- name: Sammy Sosa
  hr: 63
  avg: 0.288") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "Mark McGwire: {hr: 65, avg: 0.278}
Sammy Sosa: {
    hr: 63,
    avg: 0.288
  }" "Mark McGwire: { hr: 65, avg: 0.278 }
Sammy Sosa: { hr: 63, avg: 0.288 }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "Mark McGwire: {hr: 65, avg: 0.278}
Sammy Sosa: {
    hr: 63,
    avg: 0.288
  }" "Mark McGwire: { hr: 65, avg: 0.278 }
Sammy Sosa: { hr: 63, avg: 0.288 }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals" "# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals" "# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
hr: # 1998 hr ranking
  - Mark McGwire
  - Sammy Sosa
rbi:
  # 1998 rbi ranking
  - Sammy Sosa
  - Ken Griffey" "---
hr: # 1998 hr ranking
  - Mark McGwire
  - Sammy Sosa
rbi:
  # 1998 rbi ranking
  - Sammy Sosa
  - Ken Griffey") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
hr: # 1998 hr ranking
  - Mark McGwire
  - Sammy Sosa
rbi:
  # 1998 rbi ranking
  - Sammy Sosa
  - Ken Griffey" "---
hr: # 1998 hr ranking
  - Mark McGwire
  - Sammy Sosa
rbi:
  # 1998 rbi ranking
  - Sammy Sosa
  - Ken Griffey") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey" "---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey" "---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
# Products purchased
- item    : Super Hoop
  quantity: 1
- item    : Basketball
  quantity: 4
- item    : Big Shoes
  quantity: 1" "---
# Products purchased
- item: Super Hoop
  quantity: 1
- item: Basketball
  quantity: 4
- item: Big Shoes
  quantity: 1") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
# Products purchased
- item    : Super Hoop
  quantity: 1
- item    : Basketball
  quantity: 4
- item    : Big Shoes
  quantity: 1" "---
# Products purchased
- item: Super Hoop
  quantity: 1
- item: Basketball
  quantity: 4
- item: Big Shoes
  quantity: 1") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# ASCII Art
--- |
  \\\\//||\\\\/||
  // ||  ||__" "# ASCII Art
---
|
  \\\\//||\\\\/||
  // ||  ||__") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# ASCII Art
--- |
  \\\\//||\\\\/||
  // ||  ||__" "# ASCII Art
---
|
  \\\\//||\\\\/||
  // ||  ||__") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "--- >
  Mark McGwire's
  year was crippled
  by a knee injury." "---
>
  Mark McGwire's
  year was crippled
  by a knee injury.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) ">
 Sammy Sosa completed another
 fine season with great stats.

   63 Home Runs
   0.288 Batting Average

 What a year!" ">
  Sammy Sosa completed another
  fine season with great stats.

    63 Home Runs
    0.288 Batting Average

  What a year!") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "name: Mark McGwire
accomplishment: >
  Mark set a major league
  home run record in 1998.
stats: |
  65 Home Runs
  0.278 Batting Average" "name: Mark McGwire
accomplishment: >
  Mark set a major league
  home run record in 1998.
stats: |
  65 Home Runs
  0.278 Batting Average") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "plain:
  This unquoted scalar
  spans many lines.

quoted: \"So does this
  quoted scalar.\\\\n\"" "plain: This unquoted scalar
  spans many lines.

quoted: \"So does this
  quoted scalar.\\\\n\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "---
not-date: !!str 2002-04-28

picture: !!binary |
 R0lGODlhDAAMAIQAAP//9/X
 17unp5WZmZgAAAOfn515eXv
 Pz7Y6OjuDg4J+fn5OTk6enp
 56enmleECcgggoBADs=

application specific tag: !something |
 The semantics of the tag
 above may be different for
 different documents." "---
not-date: !!str 2002-04-28

picture: !!binary |
  R0lGODlhDAAMAIQAAP//9/X
  17unp5WZmZgAAAOfn515eXv
  Pz7Y6OjuDg4J+fn5OTk6enp
  56enmleECcgggoBADs=

application specific tag: !something |
  The semantics of the tag
  above may be different for
  different documents.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
not-date: !!str 2002-04-28

picture: !!binary |
 R0lGODlhDAAMAIQAAP//9/X
 17unp5WZmZgAAAOfn515eXv
 Pz7Y6OjuDg4J+fn5OTk6enp
 56enmleECcgggoBADs=

application specific tag: !something |
 The semantics of the tag
 above may be different for
 different documents." "---
not-date: !!str 2002-04-28

picture: !!binary |
  R0lGODlhDAAMAIQAAP//9/X
  17unp5WZmZgAAAOfn515eXv
  Pz7Y6OjuDg4J+fn5OTk6enp
  56enmleECcgggoBADs=

application specific tag: !something |
  The semantics of the tag
  above may be different for
  different documents.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%TAG ! tag:clarkevans.com,2002:
--- !shape
  # Use the ! handle for presenting
  # tag:clarkevans.com,2002:circle
- !circle
  center: &ORIGIN {x: 73, y: 129}
  radius: 7
- !line
  start: *ORIGIN
  finish: { x: 89, y: 102 }
- !label
  start: *ORIGIN
  color: 0xFFEEBB
  text: Pretty vector drawing." "%TAG ! tag:clarkevans.com,2002:
---
!shape
# Use the ! handle for presenting
# tag:clarkevans.com,2002:circle
- !circle
  center: &ORIGIN { x: 73, y: 129 }
  radius: 7
- !line
  start: *ORIGIN
  finish: { x: 89, y: 102 }
- !label
  start: *ORIGIN
  color: 0xFFEEBB
  text: Pretty vector drawing.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%TAG ! tag:clarkevans.com,2002:
--- !shape
  # Use the ! handle for presenting
  # tag:clarkevans.com,2002:circle
- !circle
  center: &ORIGIN {x: 73, y: 129}
  radius: 7
- !line
  start: *ORIGIN
  finish: { x: 89, y: 102 }
- !label
  start: *ORIGIN
  color: 0xFFEEBB
  text: Pretty vector drawing." "%TAG ! tag:clarkevans.com,2002:
---
!shape
# Use the ! handle for presenting
# tag:clarkevans.com,2002:circle
- !circle
  center: &ORIGIN { x: 73, y: 129 }
  radius: 7
- !line
  start: *ORIGIN
  finish: { x: 89, y: 102 }
- !label
  start: *ORIGIN
  color: 0xFFEEBB
  text: Pretty vector drawing.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# Sets are represented as a
# Mapping where each key is
# associated with a null value
--- !!set
? Mark McGwire
? Sammy Sosa
? Ken Griff" "# Sets are represented as a
# Mapping where each key is
# associated with a null value
---
!!set
? Mark McGwire
? Sammy Sosa
? Ken Griff") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# Sets are represented as a
# Mapping where each key is
# associated with a null value
--- !!set
? Mark McGwire
? Sammy Sosa
? Ken Griff" "# Sets are represented as a
# Mapping where each key is
# associated with a null value
---
!!set
? Mark McGwire
? Sammy Sosa
? Ken Griff") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
--- !!omap
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58" "# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
---
!!omap
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
--- !!omap
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58" "# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
---
!!omap
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "--- !<tag:clarkevans.com,2002:invoice>
invoice: 34843
date   : 2001-01-23
bill-to: &id001
    given  : Chris
    family : Dumars
    address:
        lines: |
            458 Walkman Dr.
            Suite #292
        city    : Royal Oak
        state   : MI
        postal  : 48046
ship-to: *id001
product:
    - sku         : BL394D
      quantity    : 4
      description : Basketball
      price       : 450.00
    - sku         : BL4438H
      quantity    : 1
      description : Super Hoop
      price       : 2392.00
tax  : 251.42
total: 4443.52
comments:
    Late afternoon is best.
    Backup contact is Nancy
    Billsmer @ 338-4338." "---
!<tag:clarkevans.com,2002:invoice>
invoice: 34843
date: 2001-01-23
bill-to: &id001
  given: Chris
  family: Dumars
  address:
    lines: |
      458 Walkman Dr.
      Suite #292
    city: Royal Oak
    state: MI
    postal: 48046
ship-to: *id001
product:
  - sku: BL394D
    quantity: 4
    description: Basketball
    price: 450.00
  - sku: BL4438H
    quantity: 1
    description: Super Hoop
    price: 2392.00
tax: 251.42
total: 4443.52
comments: Late afternoon is best.
  Backup contact is Nancy
  Billsmer @ 338-4338.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---
Time: 2001-11-23 15:01:42 -5
User: ed
Warning:
  This is an error message
  for the log file
---
Time: 2001-11-23 15:02:31 -5
User: ed
Warning:
  A slightly different error
  message.
---
Date: 2001-11-23 15:03:17 -5
User: ed
Fatal:
  Unknown variable \"bar\"
Stack:
  - file: TopClass.py
    line: 23
    code: |
      x = MoreObject(\"345\\\\n\")
  - file: MoreClass.py
    line: 58
    code: |-
      foo = bar" "---
Time: 2001-11-23 15:01:42 -5
User: ed
Warning: This is an error message
  for the log file
---
Time: 2001-11-23 15:02:31 -5
User: ed
Warning: A slightly different error
  message.
---
Date: 2001-11-23 15:03:17 -5
User: ed
Fatal: Unknown variable \"bar\"
Stack:
  - file: TopClass.py
    line: 23
    code: |
      x = MoreObject(\"345\\\\n\")
  - file: MoreClass.py
    line: 58
    code: |-
      foo = bar") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "sequence: [ one, two, ]
mapping: { sky: blue, sea: green }" "sequence: [one, two]
mapping: { sky: blue, sea: green }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "sequence: [ one, two, ]
mapping: { sky: blue, sea: green }" "sequence: [one, two]
mapping: { sky: blue, sea: green }") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# Comment only." "# Comment only.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# Comment only." "# Comment only.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "anchored: !local &anchor value
alias: *anchor" "anchored: !local &anchor value
alias: *anchor") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "anchored: !local &anchor value
alias: *anchor" "anchored: !local &anchor value
alias: *anchor") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "literal: |
  some
  text
folded: >
  some
  text" "literal: |
  some
  text
folded: >
  some
  text") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "single: 'text'
double: \"text\"" "single: \"text\"
double: \"text\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "single: 'text'
double: \"text\"" "single: \"text\"
double: \"text\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%YAML 1.2
--- text" "%YAML 1.2
---
text") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%YAML 1.2
--- text" "%YAML 1.2
---
text") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- foo:	 bar
- - baz
  -	baz" "- foo: bar
- - baz
  - baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- foo:	 bar
- - baz
  -	baz" "- foo: bar
- - baz
  - baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "Folding:
  \"Empty line

  as a line feed\"
Chomping: |
  Clipped empty lines" "Folding: \"Empty line

  as a line feed\"
Chomping: |
  Clipped empty lines") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "Folding:
  \"Empty line

  as a line feed\"
Chomping: |
  Clipped empty lines" "Folding: \"Empty line

  as a line feed\"
Chomping: |
  Clipped empty lines") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) ">-
  trimmed



  as
  space" ">-
  trimmed



  as
  space") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) ">
  foo

  	 bar

  baz" ">
  foo

  	 bar

  baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) ">
  foo

  	 bar

  baz" ">
  foo

  	 bar

  baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "key:    # Comment
  value" "key: # Comment
  value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "key:    # Comment
  value" "key: # Comment
  value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# Comment" "# Comment") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# Comment" "# Comment") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "key:    # Comment
        # lines
  value" "key: # Comment
  # lines
  value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "key:    # Comment
        # lines
  value" "key: # Comment
  # lines
  value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "{ first: Sammy, last: Sosa }:
# Statistics:
  hr:  # Home runs
     65
  avg: # Average
   0.278" "{ first: Sammy, last: Sosa }:
  # Statistics:
  hr: # Home runs
    65
  avg: # Average
    0.278") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "{ first: Sammy, last: Sosa }:
# Statistics:
  hr:  # Home runs
     65
  avg: # Average
   0.278" "{ first: Sammy, last: Sosa }:
  # Statistics:
  hr: # Home runs
    65
  avg: # Average
    0.278") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%FOO  bar baz # Should be ignored
              # with a warning.
--- \"foo\"" "%FOO bar baz # Should be ignored
# with a warning.
---
\"foo\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%FOO  bar baz # Should be ignored
              # with a warning.
--- \"foo\"" "%FOO bar baz # Should be ignored
# with a warning.
---
\"foo\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%YAML 1.3 # Attempt parsing
          # with a warning
---
\"foo\"" "%YAML 1.3 # Attempt parsing
# with a warning
---
\"foo\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%YAML 1.3 # Attempt parsing
          # with a warning
---
\"foo\"" "%YAML 1.3 # Attempt parsing
# with a warning
---
\"foo\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%TAG !yaml! tag:yaml.org,2002:
---
!yaml!str \"foo\"" "%TAG !yaml! tag:yaml.org,2002:
---
!yaml!str \"foo\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%TAG !yaml! tag:yaml.org,2002:
---
!yaml!str \"foo\"" "%TAG !yaml! tag:yaml.org,2002:
---
!yaml!str \"foo\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# Private
!foo \"bar\"
...
# Global
%TAG ! tag:example.com,2000:app/
---
!foo \"bar\"" "# Private
!foo \"bar\"
...
# Global
%TAG ! tag:example.com,2000:app/
---
!foo \"bar\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# Private
!foo \"bar\"
...
# Global
%TAG ! tag:example.com,2000:app/
---
!foo \"bar\"" "# Private
!foo \"bar\"
...
# Global
%TAG ! tag:example.com,2000:app/
---
!foo \"bar\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%TAG !! tag:example.com,2000:app/
---
!!int 1 - 3 # Interval, not integer" "%TAG !! tag:example.com,2000:app/
---
!!int 1 - 3 # Interval, not integer") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%TAG !! tag:example.com,2000:app/
---
!!int 1 - 3 # Interval, not integer" "%TAG !! tag:example.com,2000:app/
---
!!int 1 - 3 # Interval, not integer") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%TAG !e! tag:example.com,2000:app/
---
!e!foo \"bar\"" "%TAG !e! tag:example.com,2000:app/
---
!e!foo \"bar\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%TAG !e! tag:example.com,2000:app/
---
!e!foo \"bar\"" "%TAG !e! tag:example.com,2000:app/
---
!e!foo \"bar\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%TAG !e! tag:example.com,2000:app/
---
- !e!foo \"bar\"" "%TAG !e! tag:example.com,2000:app/
---
- !e!foo \"bar\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%TAG !e! tag:example.com,2000:app/
---
- !e!foo \"bar\"" "%TAG !e! tag:example.com,2000:app/
---
- !e!foo \"bar\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "!!str &a1 \"foo\":
  !!str bar
&a2 baz : *a1" "!!str &a1 \"foo\": !!str bar
&a2 baz: *a1") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "!!str &a1 \"foo\":
  !!str bar
&a2 baz : *a1" "!!str &a1 \"foo\": !!str bar
&a2 baz: *a1") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "!<tag:yaml.org,2002:str> foo :
  !<!bar> baz" "!<tag:yaml.org,2002:str> foo: !<!bar> baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "!<tag:yaml.org,2002:str> foo :
  !<!bar> baz" "!<tag:yaml.org,2002:str> foo: !<!bar> baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%TAG !e! tag:example.com,2000:app/
---
- !local foo
- !!str bar
- !e!tag%21 baz" "%TAG !e! tag:example.com,2000:app/
---
- !local foo
- !!str bar
- !e!tag%21 baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%TAG !e! tag:example.com,2000:app/
---
- !local foo
- !!str bar
- !e!tag%21 baz" "%TAG !e! tag:example.com,2000:app/
---
- !local foo
- !!str bar
- !e!tag%21 baz") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# Assuming conventional resolution:
- \"12\"
- 12
- ! 12" "# Assuming conventional resolution:
- \"12\"
- 12
- ! 12") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# Assuming conventional resolution:
- \"12\"
- 12
- ! 12" "# Assuming conventional resolution:
- \"12\"
- 12
- ! 12") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "First occurrence: &anchor Value
Second occurrence: *anchor" "First occurrence: &anchor Value
Second occurrence: *anchor") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "First occurrence: &anchor Value
Second occurrence: *anchor" "First occurrence: &anchor Value
Second occurrence: *anchor") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "First occurrence: &anchor Foo
Second occurrence: *anchor
Override anchor: &anchor Bar
Reuse anchor: *anchor" "First occurrence: &anchor Foo
Second occurrence: *anchor
Override anchor: &anchor Bar
Reuse anchor: *anchor") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "First occurrence: &anchor Foo
Second occurrence: *anchor
Override anchor: &anchor Bar
Reuse anchor: *anchor" "First occurrence: &anchor Foo
Second occurrence: *anchor
Override anchor: &anchor Bar
Reuse anchor: *anchor") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "\"folded
to a space,

to a line feed, or 	\\\\
 \\\\ 	non-content\"" "\"folded
to a space,

to a line feed, or 	\\\\
\\\\ 	non-content\"") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# Outside flow collection:
- ::vector
- \": - ()\"
- Up, up, and away!
- -123
- http://example.com/foo#bar
# Inside flow collection:
- [ ::vector,
  \": - ()\",
  \"Up, up and away!\",
  -123,
  http://example.com/foo#bar ]" "# Outside flow collection:
- ::vector
- \": - ()\"
- Up, up, and away!
- -123
- http://example.com/foo#bar
# Inside flow collection:
- [::vector, \": - ()\", \"Up, up and away!\", -123, http://example.com/foo#bar]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# Outside flow collection:
- ::vector
- \": - ()\"
- Up, up, and away!
- -123
- http://example.com/foo#bar
# Inside flow collection:
- [ ::vector,
  \": - ()\",
  \"Up, up and away!\",
  -123,
  http://example.com/foo#bar ]" "# Outside flow collection:
- ::vector
- \": - ()\"
- Up, up, and away!
- -123
- http://example.com/foo#bar
# Inside flow collection:
- [::vector, \": - ()\", \"Up, up and away!\", -123, http://example.com/foo#bar]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "[
foo: bar
]" "[foo: bar]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "[
foo: bar
]" "[foo: bar]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- [ a, b ]
- { a: b }
- \"a\"
- 'b'
- c" "- [a, b]
- { a: b }
- \"a\"
- \"b\"
- c") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- [ a, b ]
- { a: b }
- \"a\"
- 'b'
- c" "- [a, b]
- { a: b }
- \"a\"
- \"b\"
- c") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- !!str \"a\"
- 'b'
- &anchor \"c\"
- *anchor
- !!str" "- !!str \"a\"
- \"b\"
- &anchor \"c\"
- *anchor
- !!str") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- !!str \"a\"
- 'b'
- &anchor \"c\"
- *anchor
- !!str" "- !!str \"a\"
- \"b\"
- &anchor \"c\"
- *anchor
- !!str") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- | # Empty header↓
 literal
- >1 # Indentation indicator↓
  folded
- |+ # Chomping indicator↓
 keep

- >1- # Both indicators↓
  strip" "- | # Empty header↓
  literal
- >1 # Indentation indicator↓
  folded
- |+ # Chomping indicator↓
  keep

- >1- # Both indicators↓
  strip") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- | # Empty header↓
 literal
- >1 # Indentation indicator↓
  folded
- |+ # Chomping indicator↓
 keep

- >1- # Both indicators↓
  strip" "- | # Empty header↓
  literal
- >1 # Indentation indicator↓
  folded
- |+ # Chomping indicator↓
  keep

- >1- # Both indicators↓
  strip") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- |
 detected
- >


  # detected
- |1
  explicit
- >

 detected" "- |
  detected
- >


  # detected
- |1
  explicit
- >

  detected") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- |
 detected
- >


  # detected
- |1
  explicit
- >

 detected" "- |
  detected
- >


  # detected
- |1
  explicit
- >

  detected") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "strip: |-
  text
clip: |
  text
keep: |+
  text" "strip: |-
  text
clip: |
  text
keep: |+
  text") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "strip: |-
  text
clip: |
  text
keep: |+
  text" "strip: |-
  text
clip: |
  text
keep: |+
  text") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "# Strip
  # Comments:
strip: |-
  # text

 # Clip
  # comments:

clip: |
  # text

 # Keep
  # comments:

keep: |+
  # text

 # Trail
  # comments." "# Strip
# Comments:
strip: |-
  # text

# Clip
# comments:

clip: |
  # text

# Keep
# comments:

keep: |+
  # text

# Trail
# comments.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "# Strip
  # Comments:
strip: |-
  # text

 # Clip
  # comments:

clip: |
  # text

 # Keep
  # comments:

keep: |+
  # text

 # Trail
  # comments." "# Strip
# Comments:
strip: |-
  # text

# Clip
# comments:

clip: |
  # text

# Keep
# comments:

keep: |+
  # text

# Trail
# comments.") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "strip: >-

clip: >

keep: |+" "strip: >-

clip: >

keep: |+") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "strip: >-

clip: >

keep: |+" "strip: >-

clip: >

keep: |+") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "|
 literal
 	text" "|
  literal
  	text") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "|
 literal
 	text" "|
  literal
  	text") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "|


  literal


  text

 # Comment" "|


  literal


  text

# Comment") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "|


  literal


  text

 # Comment" "|


  literal


  text

# Comment") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) ">
 folded
 text" ">
  folded
  text") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) ">

 folded
 line

 next
 line
   * bullet

   * list
   * lines

 last
 line

# Comment" ">

  folded
  line

  next
  line
    * bullet

    * list
    * lines

  last
  line

# Comment") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "block sequence:
  - one
  - two : three" "block sequence:
  - one
  - two: three") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "block sequence:
  - one
  - two : three" "block sequence:
  - one
  - two: three") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "block mapping:
 key: value" "block mapping:
  key: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "block mapping:
 key: value" "block mapping:
  key: value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "? explicit key # Empty value
? |
  block key
: - one # Explicit compact
  - two # block value" "? explicit key # Empty value
? |
  block key
: - one # Explicit compact
  - two # block value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "? explicit key # Empty value
? |
  block key
: - one # Explicit compact
  - two # block value" "? explicit key # Empty value
? |
  block key
: - one # Explicit compact
  - two # block value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "plain key: in-line value
: # Both empty
\"quoted key\":
- entry" "plain key: in-line value
: # Both empty
\"quoted key\":
  - entry") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "plain key: in-line value
: # Both empty
\"quoted key\":
- entry" "plain key: in-line value
: # Both empty
\"quoted key\":
  - entry") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "- sun: yellow
- ? earth: blue
  : moon: white" "- sun: yellow
- ? earth: blue
  : moon: white") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "- sun: yellow
- ? earth: blue
  : moon: white" "- sun: yellow
- ? earth: blue
  : moon: white") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "-
  \"flow in block\"
- >
 Block scalar
- !!map # Block collection
  foo : bar" "- \"flow in block\"
- >
  Block scalar
- !!map # Block collection
  foo: bar") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "-
  \"flow in block\"
- >
 Block scalar
- !!map # Block collection
  foo : bar" "- \"flow in block\"
- >
  Block scalar
- !!map # Block collection
  foo: bar") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "sequence: !!seq
- entry
- !!seq
 - nested
mapping: !!map
 foo: bar" "sequence: !!seq
  - entry
  - !!seq
    - nested
mapping: !!map
  foo: bar") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "sequence: !!seq
- entry
- !!seq
 - nested
mapping: !!map
 foo: bar" "sequence: !!seq
  - entry
  - !!seq
    - nested
mapping: !!map
  foo: bar") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "%YAML 1.2
---
Document
... # Suffix" "%YAML 1.2
---
Document
... # Suffix") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "%YAML 1.2
---
Document
... # Suffix" "%YAML 1.2
---
Document
... # Suffix") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "{}" "{}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "{}" "{}") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "foo: !!seq
  - !!str a
  - !!map
    key: !!str value" "foo: !!seq
  - !!str a
  - !!map
    key: !!str value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "foo: !!seq
  - !!str a
  - !!map
    key: !!str value" "foo: !!seq
  - !!str a
  - !!map
    key: !!str value") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "!!str a: b
c: !!int 42
e: !!str f
g: h
!!int 23: !!bool false" "!!str a: b
c: !!int 42
e: !!str f
g: h
!!int 23: !!bool false") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "!!str a: b
c: !!int 42
e: !!str f
g: h
!!int 23: !!bool false" "!!str a: b
c: !!int 42
e: !!str f
g: h
!!int 23: !!bool false") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "---word1
word2" "---word1
word2") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "&flowseq [
 a: b,
 &c c: d,
 { &e e: f },
 &g { g: h }
]" "&flowseq [a: b, &c c: d, { &e e: f }, &g { g: h }]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "&flowseq [
 a: b,
 &c c: d,
 { &e e: f },
 &g { g: h }
]" "&flowseq [a: b, &c c: d, { &e e: f }, &g { g: h }]") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (proseWrap . #("always" 0 6 (fontified nil)))) "a: b
seq:
 - a
c: d	#X" "a: b
seq:
  - a
c: d #X") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "a: b
seq:
 - a
c: d	#X" "a: b
seq:
  - a
c: d #X") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "--- >
line1
line2
line3" "---
>
  line1
  line2
  line3") (((parsers . [#("yaml" 0 4 (fontified nil))]) (printWidth . 80) (useTabs . t)) "--- >
line1
# no comment
line3" "---
>
  line1
  # no comment
  line3")))

;; Notes of how many tests contained in prettier-test-cases.txt have passed.
;; 2-12: 251/718
;; 2-12: 295/718
;; 2-12: 300/718
;; 2-12: 304/718
;; 2-12: 308/718
;; 2-13: 324/718
;; 2-13: 340/718
;; 2-13: 342/718
;; 2-13: 343/718
;; 2-14: 351/718
;; 2-15: 354/718
;; 2-15: 361/718
;; 2-15: 363/718
;; 2-15: 365/718
;; 2-15: 369/718
;; 2-15: 364/718
;; 2-15: 367/718
;; 2-15: 380/718
;; 2-15: 383/718
;; 2-15: 385/718

(provide 'yaml-pro-format-ts-tests)
;;; yaml-pro-format-ts-tests.el ends here
