# lean-mode-contrib

Installlation
=========

With Quelpa
==========

If you use [Quelpa](https://github.com/quelpa/quelpa), add the following to your `.emacs` or `init.el` file:

```
(quelpa '(lean-diff-types :repo "lean-community/lean-mode-contrib" :fetcher github))
```

Without Quelpa
===========

1. Copy the files in a directory such as `~/.emacs.d/lisp/`
2. Make sure that directory is in your emacs `load-path` by putting

```
(setq load-path (cons "~/.emacs.d/lisp/" load-path))
```

in your `.emacs` or `init.el` file.

3. Add `(require 'lean-diff-types)` to your `.emacs` or `init.el` file

Key Bindings and Commands
=========================


| Key                | Function                                                                        |
|--------------------|---------------------------------------------------------------------------------|
| <kbd>C-c C-t</kbd> | apply `diff` to the expected / actual type in error messages
