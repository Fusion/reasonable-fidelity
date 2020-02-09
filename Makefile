.DEFAULT_GOAL := help

CURSES_PKG_NAME=ocaml-curses-1.0.3.ogunden1.tar.gz

prepare: prepare_curses prepare_lymp ##@dependencies Prepare (most) dependencies

prepare_curses: prepare_tools ocaml-curses-1.0.3 ocaml-curses-1.0.3/patched curses_howto

prepare_lymp: prepare_tools lymp lymp/patched lymp_howto

prepare_tools:
ifeq (,$(shell which curl))
ifeq (,$(shell which wget2))
	@echo "\n# Please, install curl or wget #\n"
	exit 1
else
	$(eval getter := wget)
endif
else
	$(eval getter := curl -O)
endif
ifeq (,$(shell which autoreconf))
	@echo "\n# Please, install autoconf/autoreconf #\n"
	exit 1
endif
ifeq (,$(shell which make))
	@echo "\n# Please, install make and build essential #\n"
	exit 1
endif

ocaml-curses: ocaml-curses-1.0.3 ##@dependencies Build patched curses library

ocaml-curses-1.0.3:
	@echo "\n# Downloading curses source code file\n" && \
	$(getter) http://ocaml.phauna.org/distfiles/$(CURSES_PKG_NAME) && \
	tar zxvf $(CURSES_PKG_NAME) && \
	rm $(CURSES_PKG_NAME)

ocaml-curses-1.0.3/patched:
	@echo "\n# Patching curses source code file\n" && \
	cd ocaml-curses-1.0.3 && \
	autoreconf && \
	sed -i '/if test -n "$$TERM_H_STRING"/i   TERM_H_STRING="<term.h>"' configure && \
	./configure --enable-widec && \
	touch patched

curses_howto:
	@echo "\nIf you are using this patched curses version\n(all the patch does is lie about 'term.h')" && \
	echo "then do not forget to:\n\n    cd ocaml-curses-1.0.3\n    make all opt install\n"

lymp: ##@dependencies Build patched python linking library
	@echo "\n# Cloning lymp source code\n" && \
	gitt clone https://github.com/dbousque/lymp.git

lymp/patched:
	@echo "\n# Patching lymp source code\n" && \
	cd lymp && \
	sed -i 's/Pervasives/Stdlib/' setup.ml && \
	touch patched

lymp_howto:
	@echo "\nIf you are using this patched lymp version\n(the patch moves to Stdlib for 4.09)" && \
	echo "then do not forget to:\n\n    cd lymp\n    make all install\n"

link_stubs: ##@dependencies Create usable symlinks to dependencies' stub libraries
	sudo bash -c 'for lib in ~/.opam/default/lib/stublibs/*; do libname="$$(basename $$lib)"; ln -s $$lib /usr/local/lib/$${libname/dll/lib}; done' | true

clean: ##@building Clean build data
	dune clean

build: ##@building Build all executables
	dune build

rebuild: ##@building Build with enhanced context output (requires Refmterr)
	refmterr dune build

release: releases/reasonable-fidelity-macos releases/reasonable-fidelity-linux ##@building Cross-platforms, release

releases/reasonable-fidelity-macos:
	@echo "Storing macos release in releases/" && \
	cp _build/default/src/main.exe releases/reasonable-fidelity-macos

releases/reasonable-fidelity-linux:
	@echo "Storing linux release in releases/" && \
	docker cp ubuntu-headless:/home/chris/reasonable-fidelity/_build/default/src/main.exe releases/reasonable-fidelity-linux

run: build ##@running Run main executable
	./_build/install/default/bin/reasonablefidelity run

runtests: build ##@@running Run tests in test/ directory
	./_build/install/default/bin/RunTests

GREEN  := $(shell tput -Txterm setaf 2)
WHITE  := $(shell tput -Txterm setaf 7)
YELLOW := $(shell tput -Txterm setaf 3)
RESET  := $(shell tput -Txterm sgr0)
HELP_FUN = \
    %help; while(<>) { push @{$$help{$$2 // 'options'}}, [$$1, $$3] if /^([a-zA-Z\-]+)\s*:.*\#\#(?:@([a-zA-Z\-]+))?\s(.*)$$/ }; \
    print "Usage: make [target]\n\n"; for (sort keys %help) { print "${WHITE}$$_:${RESET}\n"; for (@{$$help{$$_}}) { \
    $$sep = " " x (32 - length $$_->[0]); print "  ${YELLOW}$$_->[0]${RESET}$$sep${GREEN}$$_->[1]${RESET}\n"; }; print "\n"; }

help: ##@other Show this help.
	@perl -e '$(HELP_FUN)' $(MAKEFILE_LIST)

.PHONY: help clean build rebuild link_stubs curses_howto lymp_howto prepare prepare_curses prepare_lymp run runtests
