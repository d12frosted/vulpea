.PHONY: clean
clean:
	eldev clean all

.PHONY: prepare
prepare:
	eldev --unstable -p -dtT prepare

.PHONY: lint
lint:
	eldev --unstable -p -dtT lint

.PHONY: test
test:
	eldev --unstable -p -dtT test
