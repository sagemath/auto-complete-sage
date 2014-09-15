# Overview

`auto-complete-sage` provides an [auto-complete](https://github.com/auto-complete/auto-complete)
 source for [sage-shell-mode](https://github.com/stakemori/sage-shell-mode).

![ac-sage](images/ac-sage.gif)

# Installation
You will be able to install `auto-complete-sage` from
[MELPA](https://github.com/milkypostman/melpa.git) by package.el
(`M-x package-install auto-complete-sage`).

To add sources to `ac-sources`, put the following lines to `"~/.emacs.d/init.el"`:
```lisp
(add-hook 'sage-shell:sage-mode-hook 'ac-sage-setup)
(add-hook 'sage-shell-mode-hook 'ac-sage-setup)
```

# Tips
To complete global Sage objects in a buffer whose major mode is
`sage-shell:sage-shell-mode`,
run the Sage process and set the process buffer by
`M-x sage-shell:set-process-buffer` in the `sage-shell:sage-shell-mode`
 buffer.
