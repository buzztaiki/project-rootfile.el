TARGETS = project-rootfile.elc

default: compile test

%.elc: %.el
	emacs -Q -batch -L . --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile $<

.PHONY: clean
	rm *.elc

.PHONY: compile
compile: $(TARGETS)

.PHONY: test
test:
	emacs -Q -batch -L . -l ert -l project-rootfile-tests -f ert-run-tests-batch-and-exit
