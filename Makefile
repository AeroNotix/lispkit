LISP ?= sbcl
APP_NAME = lispkit
PKG_NAME = lispkit-browser
DEPLOY_HOST = zerolength.com
DEPLOY_DIR = /srv/http/bin
SCP_DEPLOY = $(DEPLOY_HOST):$(DEPLOY_DIR)
SOURCES := $(wildcard *.lisp)
PKGBUILD_FILE = PKGBUILD
PKGVER=$(shell grep -oP 'pkgver=\K([0-9]+)' $(PKGBUILD_FILE))
PKGREL=$(shell grep -oP 'pkgrel=\K([0-9]+)' $(PKGBUILD_FILE))
AURBALL= $(PKG_NAME)-$(PKGVER)-$(PKGREL).src.tar.gz
sbcl_BUILD_OPTS=--load ./make-image.lisp
clisp_BUILD_OPTS=-on-error exit < ./make-image.lisp
sbcl_TEST_OPTS=--noinform --disable-debugger --quit --load ./run-tests.lisp

.PHONY: all test

all: $(APP_NAME)

deploy: $(APP_NAME).tar.gz
	rsync -a $< $(SCP_DEPLOY)

aur-package: deploy
	sed -i 's/:md5sum/$(shell md5sum $(APP_NAME).tar.gz | cut -d' ' -f1)/g' $(PKGBUILD_FILE) && \
		makepkg -sf && \
		mkaurball -f && \
		burp $(AURBALL) && \
		git checkout $(PKGBUILD_FILE)

$(APP_NAME): $(SOURCES)
	@$(LISP) $($(LISP)_BUILD_OPTS)

test:
	@$(LISP) $($(LISP)_TEST_OPTS)

tar: lispkit.tar.gz

$(APP_NAME).tar.gz: lispkit
	tar zcvf lispkit.tar.gz lispkit
