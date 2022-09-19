# yaml-pro: tools for editing YAML leveraging a parser

yaml-pro is a package that provides conveniences for editing yaml.  It
utilizes the YAML parser at https://github.com/zkry/yaml.el to obtain
a parse tree and is then able to do things like move between subtrees,
delete entire sections of YAML (even if it's written in JSON style),
and swap subtrees.

# Demo

### Editing text in detached buffer

![screenshot](./docs/screenshot1.gif)

Never have to consult https://yaml-multiline.info/ again!  With
`yaml-pro-edit-scalar`, you can edit a scalar value in a detached
buffer and convert between the various styles with ease.

### Jump to heading (with consult support)

![jumping feature](./docs/yaml-pro-jump.gif)

### Moving subtrees up and down

![moving subtrees feature](./docs/move-subtree.gif)

### Folding subtrees

![folding feature](./docs/folding.gif)

### Killing subtrees

![killing feature](./docs/killing-subtree.gif)

### Indenting subtrees

![indenting feature](./docs/indenting.gif)

# Installation

You can install this package with MELPA under the id
`yaml-pro`. **IMPORTANT**: You have to have the latest version of
yaml.el installed or else this package won't work properly.  If your
noticing any errors try making sure that you have the correct version
of yaml.el installed (https://melpa.org/#/yaml).  You can see the
parser version with the variable `yaml-parser-version` and the
required version with the variable
`yaml-pro-required-yaml-parser-version`.

You can have yaml-pro-mode setup on yaml-mode loading with the
configuration: `(add-hook 'yaml-mode-hook #'yaml-pro-mode)`

# Usage

Run the command `yaml-pro-mode` to initialize the mode. From there you
have the following commands available (with default keybindings).

- **yaml-pro-kill-subtree** (<kbd>C-c</kbd> <kbd>C-x</kbd> <kbd>C-w</kbd>)
- **yaml-pro-up-level** (<kbd>C-c</kbd> <kbd>C-u</kbd>)
- **yaml-pro-next-subtree** (<kbd>C-c</kbd> <kbd>C-n</kbd>)
- **yaml-pro-prev-subtree** (<kbd>C-c</kbd> <kbd>C-p</kbd>)
- **yaml-pro-fold-at-point** (<kbd>C-c</kbd> <kbd>C-f</kbd>)
- **yaml-pro-unfold-at-point** (<kbd>C-c</kbd> <kbd>C-o</kbd>)
- **yaml-pro-indent-subtree** (<kbd>C-c ></kbd>)
- **yaml-pro-unindent-subtree** (<kbd>C-c <</kbd>)
- **yaml-pro-move-subtree-up** (<kbd>s-up</kbd>)
- **yaml-pro-move-subtree-down** (<kbd>s-down</kbd>)
- **yaml-pro-edit-scalar** (<kbd>C-c '</kbd>)
  - (use prefix argument <kbd>C-u</kbd> to supply an initialization
    command to set major mode)
- **yaml-pro-jump** (or **yaml-pro-consult-jump** if using consult)
  (<kbd>C-c C-j</kbd>)

*The default bindings are subject to change as this package is in beta*

# Configuration

Yaml.el, being an Emacs lisp parser, struggles with very large files.
You can configure the parser to parse a smaller section of the buffer
via a heuristic (probably error prone).  Set the custom variable
`yaml-pro-max-parse-size` to be the size of the buffer after which
such a heuristic is used.

# Recommendations

- Yaml-pro's features compliments LSP and will enhance your YAML
  editing capabilities even further.

# Roadmap

- [x] Edit yaml values in separate buffer (like org-edit-special)
  - [x] block options for how to store the string.
  - [x] save default init command on a path basis
- [x] Easy navigation (yaml-pro-jump)
- [x] Partial tree-parsing for large files
- [ ] Async parsing: preemptively parse document for faster editing
      (perhaps via idle timer).
- [ ] Tools to work with various template modes.  Go-templated YAML is
      very common but greatly hinders the effectiveness of tools like
      LSP.  Is there something that could be done (even if it's kind
      of hacky) to alleviate this?
- [ ] Implement internally path-at-point. (?)
- [ ] Move functionality to tree-sitter (for better error handling),
      perhaps when tree-sitter in Emacs reaches some critical mass.
- [ ] Common YAML mistakes linter
- [ ] Difficult-to-handle syntax highlighting

# Contributing

Have a suggestion for this package? Feel free to create an issue. I'd
love to hear others pain-points when editing YAML.
