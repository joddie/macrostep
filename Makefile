EMACS ?= emacs

all: macrostep.elc macrostep-c.elc

clean:
	rm -f *.elc

test: all
	$(EMACS) --batch -L . --load macrostep-test.el

sandbox: all
	$(EMACS) -Q -L . --load macrostep.elc --load macrostep-c.elc

%.elc: %.el
	$(EMACS) --batch -L . --funcall batch-byte-compile "$<"

.PHONY: test all clean
