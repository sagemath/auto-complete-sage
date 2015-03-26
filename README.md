# auto-complete-sage

[![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

## Overview

`auto-complete-sage` provides [auto-complete](https://github.com/auto-complete/auto-complete)
 sources for [sage-shell-mode](https://github.com/stakemori/sage-shell-mode).

![ac-sage](images/ac-sage.gif)

## Installation
You can install `auto-complete-sage` from
[MELPA](https://github.com/milkypostman/melpa.git) by package.el
(`M-x package-install auto-complete-sage`).

For the setting of [auto-complete](https://github.com/auto-complete/auto-complete),
see the [manual](http://cx4a.org/software/auto-complete/manual.html)
of `auto-complete-mode`.

`auto-complete-sage` provides specific sources for `sage-shell-mode`.
To add these sources to `ac-sources`, put the following lines to `"~/.emacs.d/init.el"`:
```lisp
(add-hook 'sage-shell:sage-mode-hook 'ac-sage-setup)
(add-hook 'sage-shell-mode-hook 'ac-sage-setup)
```

## Tab Completion
`auto-complete-sage` replaces the default completion function of
`sage-shell-mode` (`completion-at-point`) by `auto-complete`.
If you want to use `completion-at-point` for the Tab completion,
put the following line to `"~/.emacs.d/init.el"`:

```
(eval-after-load "auto-complete-sage"
  '(setq sage-shell:completion-function 'completion-at-point))
```


## Customization

* `ac-sage-show-quick-help` (default value: `nil`).
  To show a quick help, set this variable to `non-nil`.
  ```
  (setq ac-sage-show-quick-help t)
  ```
  By default, quick help for an instance of
  `sage.misc.lazy_import.LazyImport` is not shown.
  To show quick help for all instances, put the following line to
  `"~/.emacs.d/init.el"`.
  ```
  (setq ac-sage-quick-help-ignore-classes '(""))
  ```

* `ac-sage-complete-on-dot` (default value: `nil`).
  This variable is similar to `jedi:complete-on-dot` in
  [jedi](https://github.com/tkf/emacs-jedi).
  When `non-nil`, `auto-complete` starts when a dot is inserted.

For more customization, `M-x customize-group RET auto-complete-sage`.

[melpa-link]: http://melpa.org/#/auto-complete-sage
[melpa-stable-link]: http://stable.melpa.org/#/auto-complete-sage
[melpa-badge]: http://melpa.org/packages/auto-complete-sage-badge.svg
[melpa-stable-badge]: http://stable.melpa.org/packages/auto-complete-sage-badge.svg
