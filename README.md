# yaml-pro: tools for editing YAML leveraging a parser

![screenshot](./docs/screenshot1.gif)

*note: this package is still in beta. A lot could be done
performance-wise to make it better.*

yaml-pro is a package that provides conveniences for editing yaml.  It
utilizes the YAML parser at https://github.com/zkry/yaml.el to obtain
a parse tree and is then able to do things like move between subtrees,
delete entire sections of YAML (even if it's written in JSON style),
and swap subtrees.

# Installation

This package is not yet on MELPA so you will have to install it
locally.  **Make sure that you have YAML on the latest version** and
add yaml-pro.el to your load path.

# Usage

Run the command `yaml-pro-mode` to initialize the mode. From there you
have the following commands available (with default keybindings).

- **yaml-pro-kill-subtree** (<kbd>C-c</kbd> <kbd>C-x</kbd> <kbd>C-w</kbd>)
- **yaml-pro-up-level** (<kbd>C-c</kbd> <kbd>C-u</kbd>)
- **yaml-pro-next-subtree** (<kbd>C-c</kbd> <kbd>C-n</kbd>)
- **yaml-pro-prev-subtree** (<kbd>C-c</kbd> <kbd>C-p</kbd>)
- **yaml-pro-fold-at-point** (<kbd>C-c</kbd> <kbd>C-c</kbd>)
- **yaml-pro-unfold-at-point** (<kbd>C-c</kbd> <kbd>C-o</kbd>)
- **yaml-pro-move-subtree-up** (<kbd>s-up</kbd>)
- **yaml-pro-move-subtree-down** (<kbd>s-down</kbd>)

*This package being in beta, the default bindings are subject to change*

# Roadmap

- [ ] Edit yaml values in separate buffer (like org-edit-special)
  - [ ] block options for how to store the string.
- [ ] Async parsing: preemptively parse document for faster editing
      (perhaps via idle timer).
- [ ] Outline mode: a mode for viewing the outlined structure of the
      YAML document.
- Tools to work with various template modes.  Go-templated YAML is
      very common but greatly hinders the effectiveness of tools like
      LSP.  Is there something that could be done (even if it's kind
      of hacky) to alleviate this?
- YAML diff: may be helpful to have a tool that can diff yaml on a
  value basis
- Path information was considered but is currently not on the roadmap
  as LSP has this feature.


# Contributing

Have a suggestion for this package? Feel free to create an issue. I'd
love to hear others pain-points when editing YAML.
