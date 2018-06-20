build:
	jbuilder build

run: build
	./_build/install/default/bin/reasonable-fidelity run

.PHONY: build
