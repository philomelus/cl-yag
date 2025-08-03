LISP ?= sbcl

all: test

run:
	rlwrap $(LISP) --load run.lisp

build:
	$(LISP)	--non-interactive \
		--load cl-yag.asd \
		--eval '(ql:quickload :cl-yag)' \
		--eval '(asdf:make :cl-yag)'

test:
	$(LISP) --non-interactive \
		--load run-tests.lisp
