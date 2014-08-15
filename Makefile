LISP ?= sbcl
sbcl_BUILD_OPTS=--load ./make-image.lisp
clisp_BUILD_OPTS=-on-error exit < ./make-image.lisp


all:
	$(LISP) $($(LISP)_BUILD_OPTS)
