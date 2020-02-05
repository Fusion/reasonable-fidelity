CURSES_PKG_NAME=ocaml-curses-1.0.3.ogunden1.tar.gz

prepare_curses: prepare_tools ocaml-curses-1.0.3 ocaml-curses-1.0.3/patched curses_howto

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

build:
	dune build

release: releases/reasonable-fidelity-macos releases/reasonable-fidelity-linux

releases/reasonable-fidelity-macos:
	@echo "Storing macos release in releases/" && \
	cp _build/default/src/main.exe releases/reasonable-fidelity-macos

releases/reasonable-fidelity-linux:
	@echo "Storing linux release in releases/" && \
	docker cp ubuntu-headless:/home/chris/reasonable-fidelity/_build/default/src/main.exe releases/reasonable-fidelity-linux

run: build
	./_build/install/default/bin/reasonable-fidelity run

.PHONY: build
