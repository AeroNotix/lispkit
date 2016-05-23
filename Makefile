LISP ?= sbcl
DEBUILD = /tmp/lispkit
APP_NAME = lispkit
APP_OUT = $(shell pwd)/lispkit
PKG_NAME = lispkit-browser
BUILDAPP = ./bin/buildapp
DEBUILD_ROOT = /tmp/lispkit
DEPLOY_HOST = zerolength.com
DEPLOY_DIR = /srv/http/bin
SCP_DEPLOY = $(DEPLOY_HOST):$(DEPLOY_DIR)
SOURCES := $(wildcard *.lisp) $(wildcard *.asd)
PKGBUILD_FILE = PKGBUILD
PKGVER=$(shell grep -oP 'pkgver=\K([0-9]+)' $(PKGBUILD_FILE))
PKGREL=$(shell grep -oP 'pkgrel=\K([0-9]+)' $(PKGBUILD_FILE))
AURBALL= $(PKG_NAME)-$(PKGVER)-$(PKGREL).src.tar.gz
QUICKLISP_SCRIPT=http://beta.quicklisp.org/quicklisp.lisp
QL_LOCAL=$(PWD)/.quicklocal/quicklisp
LOCAL_OPTS=--noinform --noprint --disable-debugger --no-sysinit --no-userinit
QL_OPTS=--load $(QL_LOCAL)/setup.lisp
sbcl_BUILD_OPTS=--load ./make-image.lisp
sbcl_BUILD_OPTS-local=$(LOCAL_OPTS) $(QL_OPTS) --load ./make-image.lisp
clisp_BUILD_OPTS=-on-error exit < ./make-image.lisp
sbcl_TEST_OPTS=--noinform --disable-debugger --load $(QL_LOCAL)/setup.lisp --eval '(ql:quickload :prove)' --eval '(or (prove:run :lispkit-test) (uiop:quit -1))' --quit
DISTRO_ID=$(shell source /etc/os-release && echo $$ID)
SBCL_COMPRESSION := $(shell sbcl --noinform --eval "(when (member :sb-core-compression cl:*features*) (open \".has_image_compression\" :direction :probe :if-does-not-exist :create))" --quit)
ifeq ($(wildcard .has_image_compression),)
	SBCL_COMPRESSION_OPT =
else
	SBCL_COMPRESSION_OPT := --compress-core
endif
CONTAINER_ROOTFS ?= /var/lib/lxc/lispkit/rootfs


.PHONY: deploy clean deb-package aur-package test printvars aergia-create-container aergia-run

all: $(APP_OUT)

bin:
	@mkdir bin

clean:
	@-rm -rf $(QL_LOCAL)
	@-rm $(APP_NAME)
	@-rm deps
	@-rm .has_image_compression

clones: $(QL_LOCAL)/local-projects/cl-xkeysym $(QL_LOCAL)/local-projects/cl-webkit
	@touch $@

$(QL_LOCAL)/local-projects/cl-xkeysym:
	git clone https://github.com/AeroNotix/cl-xkeysym.git $@

$(QL_LOCAL)/local-projects/cl-webkit:
ifeq ($(DISTRO_ID),arch)
	git clone https://github.com/joachifm/cl-webkit $@
else
	git clone https://github.com/joachifm/cl-webkit $@ && cd $@ && git checkout c0c0a4 && cd -
endif

deploy: $(APP_NAME).tar.gz
	@rsync -a $< $(SCP_DEPLOY)

deb-package: $(APP_NAME)_debian.tar.gz
	@fpm -s tar -t deb $<

aur-package: deploy
	@sed -i 's/:md5sum/$(shell md5sum $(APP_NAME).tar.gz | cut -d' ' -f1)/g' $(PKGBUILD_FILE) && \
		makepkg -sf && \
		mkaurball -f && \
		burp $(AURBALL) && \
		git checkout $(PKGBUILD_FILE)

$(QL_LOCAL)/setup.lisp:
	@curl -O $(QUICKLISP_SCRIPT)
	@sbcl $(LOCAL_OPTS) \
		--load quicklisp.lisp \
		--eval '(quicklisp-quickstart:install :path "$(QL_LOCAL)")' \
		--eval '(quit)'

deps: $(QL_LOCAL)/setup.lisp
	@sbcl $(LOCAL_OPTS) $(QL_OPTS) \
	     --eval '(push "$(PWD)/" asdf:*central-registry*)' \
	     --eval '(ql:quickload :lispkit)' \
	     --eval '(quit)'
	@touch $@

install-deps: $(QL_LOCAL)/setup.lisp clones deps
	@touch $@

bin/buildapp: bin $(QL_LOCAL)/setup.lisp
	@cd $(shell sbcl $(LOCAL_OPTS) $(QL_OPTS) \
				--eval '(ql:quickload :buildapp :silent t)' \
				--eval '(format t "~A~%" (asdf:system-source-directory :buildapp))' \
				--eval '(quit)') && \
	$(MAKE) DESTDIR=$(PWD) install

$(APP_OUT): $(SOURCES) bin/buildapp $(QL_LOCAL)/setup.lisp clones install-deps
	@$(BUILDAPP) --logfile /tmp/build.log \
			--sbcl sbcl \
			--asdf-path . \
			--asdf-tree $(QL_LOCAL)/local-projects \
			--asdf-tree $(QL_LOCAL)/dists \
			--asdf-path . \
			--load-system $(APP_NAME) \
			--entry $(APP_NAME):do-main \
			$(SBCL_COMPRESS_OPT) \
			--output $(APP_OUT)

test: $(QL_LOCAL)/setup.lisp clones install-deps
	@$(LISP) $($(LISP)_TEST_OPTS)

tar: $(APP_NAME).tar.gz

$(APP_NAME).tar.gz: $(APP_OUT)
	@tar zcvf $@ lispkit

$(APP_NAME)_debian.tar.gz: $(APP_OUT)
	@mkdir -p ./opt/sbin/
	@cp lispkit ./opt/sbin/
	@tar zcvf $@ -C ./opt/sbin/ lispkit

printvars:
	@$(foreach V,$(sort $(.VARIABLES)), $(if $(filter-out environment% default automatic, $(origin $V)),$(warning $V=$($V) ($(value $V)))))

aergia-create-container:
ifndef SSHKEY
	$(error SSHKEY needs to be provided. It must be the path to the public SSH key.)
endif
	@lxc-create --name lispkit \
		--template ubuntu -- \
		-S $(SSHKEY) \
		--packages make,sbcl,libglib2.0-0,xvfb,software-properties-common,python-software-properties,python,zlib1g-dev,libxml2-dev,libxml2,linux-libc-dev,pkg-config,patch,libxrandr-dev,libxrandr2,libxslt1.1,xml-core,systemd,systemd-shim,libgtk-3-bin,libgtk-3-common,git \
		--release utopic
	@echo "ubuntu ALL=(ALL) NOPASSWD:ALL" >> $(CONTAINER_ROOTFS)/etc/sudoers

aergia-run:
ifndef SSHKEY
	$(error SSHKEY needs to be provided. It must be the path of the private SSH key.)
endif
	@aergia --clone lispkit --username ubuntu --prefix common-lisp --command "xvfb-run ./run-travis.sh" --ssh-identity $(SSHKEY)
