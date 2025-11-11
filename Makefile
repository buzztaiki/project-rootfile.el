TARGETS = project-rootfile.elc
EMACS = emacs

default: compile test

%.elc: %.el
	$(EMACS) -Q -batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $<

.PHONY: clean
clean:
	rm -f *.elc

.PHONY: compile
compile: $(TARGETS)

.PHONY: test
test:
	$(EMACS) -Q -batch -L . -l ert -l project-rootfile-tests -f ert-run-tests-batch-and-exit
