LISP ?= sbcl
sbcl_BUILD_OPTS=--load ./make-image.lisp
clisp_BUILD_OPTS=-on-error exit < ./make-image.lisp
sbcl_TEST_OPTS=--noinform --disable-debugger --quit --load ./run-tests.lisp

.PHONY: all test

all:
	@$(LISP) $($(LISP)_BUILD_OPTS)

test:
	@$(LISP) $($(LISP)_TEST_OPTS)

tar: lispkit.tar.gz

lispkit.tar.gz: lispkit
	tar zcvf lispkit.tar.gz lispkit main.ui templates/
