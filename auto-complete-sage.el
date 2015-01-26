;;; auto-complete-sage.el --- An auto-complete source for sage-shell-mode.
;; Copyright (C) 2012-2015 Sho Takemori.

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
(add-to-list 'ac-modes 'sage-shell-mode)
(add-to-list 'ac-modes 'sage-shell:sage-mode)

(defgroup auto-complete-sage nil "Group for auto-compete-sage"
  :group 'sage-shell)

(defcustom ac-sage-show-quick-help nil
  "Non-nil means show quick help of auto-complete-mode in
`sage-shell-mode' buffers and `sage-shell:sage-mode' buffers."
  :group 'auto-complete-sage
  :type 'boolean)

(defcustom ac-sage-quick-help-ignore-classes nil
  "If non-nil, this should be a list of strings.
Each string shoud be a class of Sage. When non-nil instances or methods
of these classes are ignored by `ac-quick-help'.
If the value is equal to '(\"\"), then it does not ignore anything."
  :group 'auto-complete-sage
  :type '(repeat string))

(defun ac-sage-setup-internal ()
  (when ac-sage-show-quick-help
    (set (make-local-variable 'sage-shell:init-command-list)
         ;; Send dummy code to import modules.
         (cons (format "%s('%s')"
                       (sage-shell:py-mod-func "print_short_doc_and_def")
                       "NumberField")
               sage-shell:init-command-list)))
  (sage-shell:awhen ac-sage-quick-help-ignore-classes
    (set (make-local-variable 'sage-shell:init-command-list)
         (cons (format "%s.ignore_classes = [%s]"
                       sage-shell:python-module
                       (mapconcat 'identity
                                  ac-sage-quick-help-ignore-classes ", "))
               sage-shell:init-command-list))))

(defvar ac-sage--repl-methods-cached nil)
(make-variable-buffer-local 'ac-sage--repl-methods-cached)

(defvar ac-sage--sage-commands-doc-cached nil)
(make-variable-buffer-local 'ac-sage--sage-commands-doc-cached)

(defun ac-sage--sage-commands-doc-clear-cache ()
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (setq ac-sage--sage-commands-doc-cached nil)))

(add-hook 'sage-shell:clear-command-cache-hook
          'ac-sage--sage-commands-doc-clear-cache)

(defun ac-sage--doc-clear-cache ()
  (sage-shell:with-current-buffer-safe sage-shell:process-buffer
    (setq ac-sage--repl-methods-cached nil)))

(cl-defmacro ac-sage--cache-doc (doc-func name base-name
                                          cache-var
                                          &optional (min-len 0))
  `(sage-shell:with-current-buffer-safe sage-shell:process-buffer
     (sage-shell:aif (and (> (length ,name) ,min-len)
                          (assoc-default ,name ,cache-var))
         it
       (let ((doc (,doc-func ,name ,base-name)))
         (prog1
             doc
           (setq ,cache-var
                 (cons (cons ,name doc) ,cache-var)))))))

(defun ac-sage-doc (can)
  (when ac-sage-show-quick-help
    (ac-sage--cache-doc ac-sage--doc can nil
                        ac-sage--sage-commands-doc-cached
                        ;; Short names may be re-defined.
                        4)))

(defun ac-sage-repl-sage-commands-doc (can)
  (when ac-sage-show-quick-help
    (ac-sage--cache-doc ac-sage--repl-doc can nil
                        ac-sage--sage-commands-doc-cached
                        4)))

(defun ac-sage-repl-methods-doc (can)
  (when ac-sage-show-quick-help
    (let* ((base-name
            (or (sage-shell-cpl:get 'var-base-name)
                (sage-shell:in (sage-shell-cpl:get 'interface)
                               sage-shell-interfaces:other-interfaces)))
           (name (sage-shell:aif base-name
                     (format "%s.%s" it can)
                   can)))
      (ac-sage--cache-doc ac-sage--repl-doc name base-name
                          ac-sage--repl-methods-cached))))

(defun ac-sage--repl-doc (name base-name)
  (when (sage-shell:at-top-level-and-in-sage-p)
    (ac-sage--doc name base-name)))

(defun ac-sage--doc (name base-name)
  (when (and (sage-shell:output-finished-p)
             (sage-shell:redirect-finished-p))
    (let ((doc (sage-shell:trim-left
                (sage-shell:send-command-to-string
                 (format "%s('%s'%s)"
                         (sage-shell:py-mod-func "print_short_doc_and_def")
                         name
                         (sage-shell:aif base-name
                             (format ", base_name='%s'" it)
                           ""))))))
      (unless (string= doc "")
        doc))))

(defvar ac-source-sage-methods
  '((init . ac-sage-repl-methods-init)
    (prefix . ac-sage-methods-prefix)
    (symbol . "f")
    (document . ac-sage-repl-methods-doc)
    (candidates . ac-sage-repl:candidates)
    (cache)))

(defun ac-sage-repl-methods-init ()
  (ac-sage--doc-clear-cache)
  (when (and (integerp ac-auto-start)
             (<= (- (point) (sage-shell-cpl:get 'prefix))
                ac-auto-start))
    (sage-shell-cpl:completion-init
     (sage-shell-cpl:get 'interface)
     (sage-shell-cpl:get 'var-base-name)
     (equal this-command 'auto-complete))))

(defun ac-sage-methods-prefix ()
  (let ((pfx (sage-shell-cpl:prefix)))
    (when (and pfx (sage-shell-cpl:get 'var-base-name))
      pfx)))

(defvar ac-source-sage-other-interfaces
  '((prefix . ac-sage-other-int-prefix)
    (symbol . "f")
    (init . ac-sage-repl:init)
    (candidates . ac-sage-repl:candidates)
    (cache)))

(defun ac-sage-other-int-prefix ()
  (let ((pfx (sage-shell-cpl:prefix)))
    (when (and pfx (not (string= (sage-shell-cpl:get 'interface) "sage")))
      pfx)))

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

(defun ac-sage-repl-python-kwds-candidates ()
  (when (and (string= (sage-shell-cpl:get 'interface) "sage")
             (null (sage-shell-cpl:get 'var-base-name)))
    ac-sage-repl:python-kwds))

(defvar ac-source-sage-repl-python-kwds
  '((candidates . ac-sage-repl-python-kwds-candidates)))

(defvar ac-source-repl-sage-commands
  '((document . ac-sage-repl-sage-commands-doc)
    (symbol . "f")
    (candidates . ac-sage-commands-candidates)
    (cache)))

(defvar ac-source-sage-commands
  '((init . (lambda ()
              (sage-shell-edit:set-sage-proc-buf-internal nil nil)))
    (document . ac-sage-doc)
    (candidates . ac-sage-commands-candidates)
    (symbol . "f")
    (cache)))

(defun ac-sage-repl:add-sources ()
  (setq ac-sources
        (append '(ac-source-sage-methods
                  ac-source-sage-other-interfaces
                  ac-source-sage-repl-python-kwds
                  ac-source-repl-sage-commands
                  ac-source-sage-words-in-buffers)
                ac-sources)))

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


;; sage-edit-ac
(defun ac-sage:add-sources ()
  (setq ac-sources
        (append
         ac-sources
         '(ac-source-sage-commands
           ac-source-sage-words-in-buffers))))

(defun ac-sage-commands-candidates ()
  (when (and sage-shell:process-buffer
             ;; To use source 'words-in-sage-buffers' in other intafaces
             (or (derived-mode-p 'python-mode)
                 (when (eq major-mode 'sage-shell-mode)
                   (and (string= (sage-shell-cpl:get 'interface) "sage")
                        (not (sage-shell-cpl:get 'var-base-name))))))
    (sage-shell:with-current-buffer-safe sage-shell:process-buffer
      (or (sage-shell-cpl:get-cmd-lst "sage")
          (sage-shell:update-sage-commands)))))

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
    (ac-sage-setup-internal)
    (ac-sage-repl:add-sources))
   ((eq major-mode 'sage-shell:sage-mode)
    (ac-sage:add-sources))))

(provide 'auto-complete-sage)
;;; auto-complete-sage.el ends here
