EMACS ?= emacs

all: macrostep.elc

clean:
	rm -f macrostep.elc macrostep-test.elc

test: macrostep.elc
	$(EMACS) --batch -L . --load macrostep-test.el

sandbox: macrostep.elc
	$(EMACS) -Q -L . --load macrostep.elc

%.elc: %.el
	$(EMACS) --batch --funcall batch-byte-compile "$<"

.PHONY: test all clean
