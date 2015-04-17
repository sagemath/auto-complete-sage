EMACS ?= emacs
CASK ?= $(HOME)/.cask/bin/cask

compile:
	$(CASK) exec $(EMACS) -Q -eval "(setq byte-compile-error-on-warn t)" \
	-batch -f batch-byte-compile auto-complete-sage.el

# Only tests byte compile warnings.
test: compile

clean:
	rm -f sage-shell-mode.elc

.PHONY: compile test clean
