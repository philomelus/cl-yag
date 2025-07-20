LISP ?= sbcl

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load clg.asd \
		--eval '(ql:quickload :clg)' \
		--eval '(asdf:make :clg)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
