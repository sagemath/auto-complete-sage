;;; auto-complete-sage.el --- An auto-complete source for sage-shell-mode.
;; Copyright (C) 2012-2014 Sho Takemori.

;; Author: Sho Takemori <stakemorii@gmail.com>
;; URL: https://github.com/stakemori/auto-complete-sage
;; Keywords: Sage, math, auto-complete
;; Version: 0.0.2
;; Package-Requires: ((auto-complete "1.4.0") (sage-shell-mode "0.0.4"))

;;; License
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; To setup, put the following lines to ~/.emacs.d/init.el.
;; (eval-after-load "auto-complete"
;;   '(setq ac-modes (append '(sage-shell-mode sage-shell:sage-mode) ac-modes)))
;; (add-hook 'sage-shell:sage-mode-hook 'ac-sage-setup)
;; (add-hook 'sage-shell-mode-hook 'ac-sage-setup)

;;; Code:
(require 'auto-complete)
(require 'sage-shell-mode)

(setq sage-shell:completion-function 'auto-complete)


;;; sage-shell-ac
(defvar ac-sage--repl-common
  '((init . ac-sage-repl:init)
    (candidates . ac-sage-repl:candidates)
    (cache)))

(defvar ac-source-sage-methods
  (append '((prefix . ac-sage-methods-prefix)
            (symbol . "f"))
        ac-sage--repl-common))

(defun ac-sage-methods-prefix ()
  (let ((pfx (sage-shell-cpl:prefix)))
    (when (and pfx (sage-shell-cpl:get 'var-base-name))
      pfx)))

(defvar ac-source-sage-other-interfaces
  (append '((prefix . ac-sage-other-int-prefix)
            (symbol . "f"))
        ac-sage--repl-common))

(defun ac-sage-other-int-prefix ()
  (let ((pfx (sage-shell-cpl:prefix)))
    (when (and pfx (not (string= (sage-shell-cpl:get 'interface) "sage")))
      pfx)))

(add-to-list 'ac-modes 'sage-shell-mode)

(defvar ac-source-sage-python-kwds
  '((candidates . (lambda () ac-sage-repl:python-kwds))))

(defun ac-sage-repl:add-sources ()
  (setq ac-sources
        (append '(ac-source-sage-methods
                  ac-source-sage-other-interfaces
                  ac-source-sage-commands
                  ac-source-sage-python-kwds
                  ac-source-sage-words-in-buffers)
                ac-sources)))

(defvar ac-sage-repl:python-kwds
  '("abs" "all" "and" "any" "apply" "as" "assert" "basestring"
    "bin" "bool" "break" "buffer" "bytearray" "callable" "chr"
    "class" "classmethod" "cmp" "coerce" "compile" "complex"
    "continue" "def" "del" "delattr" "dict" "dir" "divmod" "elif"
    "else" "enumerate" "eval" "except" "exec" "execfile" "file" "filter"
    "finally" "float" "for" "format" "from" "frozenset" "getattr" "global"
    "globals" "hasattr" "hash" "help" "hex" "id" "if" "import" "in" "input"
    "int" "intern" "is" "isinstance" "issubclass" "iter" "lambda" "len" "list"
    "locals" "long" "map" "max" "memoryview" "min" "next" "not" "object" "oct"
    "open" "or" "ord" "pass" "pow" "print" "property" "raise" "range" "raw"
    "reduce" "reload" "repr" "return" "reversed" "round" "set" "setattr"
    "slice" "sorted" "staticmethod" "str" "sum" "super" "try" "tuple" "type"
    "unichr" "unicode" "vars" "while" "with" "xrange" "yield" "zip" "__import__"))

(defun ac-sage-repl:init ()
  (when (sage-shell:output-finished-p)
    (sage-shell-cpl:completion-init
     (sage-shell-cpl:get 'interface)
     (sage-shell-cpl:get 'var-base-name)
     (equal this-command 'auto-complete))))

(defun ac-sage-repl:candidates ()
  (when (and (sage-shell:redirect-finished-p)
             (sage-shell:output-finished-p))
    (sage-shell-cpl:candidates)))

(defvar ac-source-sage-shell
  '((init . ac-sage-repl:init)
    (prefix . sage-shell-cpl:prefix)
    (candidates . ac-sage-repl:candidates)
    (cache)))


;; sage-edit-ac
(add-to-list 'ac-modes 'sage-shell:sage-mode)

(defun ac-sage:add-sources ()
  (setq ac-sources
        (append '(ac-source-sage-commands
                  ac-source-sage-words-in-buffers)
                ac-sources)))

(defvar ac-sage:sage-commands nil)
(make-variable-buffer-local 'ac-sage:sage-commands)

(defun ac-sage:candidates ()
  (and sage-shell:process-buffer
        (and (sage-shell:redirect-finished-p)
             (sage-shell:output-finished-p))
        (or ac-sage:sage-commands
            (setq ac-sage:sage-commands
                  (sage-shell:awhen (get-buffer sage-shell:process-buffer)
                    (with-current-buffer it
                      (or (sage-shell-cpl:get-cmd-lst "sage")
                          (sage-shell:update-sage-commands))))))))

(defvar ac-source-sage-commands
  '((init . (lambda () (sage-shell-edit:set-sage-proc-buf-internal nil)))
    (candidates . ac-sage:candidates)
    (symbol . "f")
    (cache)))

(defun ac-sage:words-in-sage-buffers ()
  (ac-word-candidates
   (lambda (buf)
     (sage-shell:in (buffer-local-value 'major-mode buf) sage-shell:sage-modes))))

(defvar ac-source-sage-words-in-buffers
  '((init . ac-update-word-index)
    (candidates . ac-sage:words-in-sage-buffers)))

;;;###autoload
(defun ac-sage-setup ()
  (interactive)
  (cond
   ((eq major-mode 'sage-shell-mode)
    (ac-sage-repl:add-sources))
   ((eq major-mode 'sage-shell:sage-mode)
    (ac-sage:add-sources))))

(provide 'auto-complete-sage)
;;; auto-complete-sage.el ends here
