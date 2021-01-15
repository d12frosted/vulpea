.PHONY: clean
clean:
	eldev clean all

.PHONY: prepare
prepare:
	eldev -C --unstable -p -dtT prepare

.PHONY: lint
lint:
	eldev -C --unstable -p -dtT lint

.PHONY: test
test:
	eldev -C --unstable -p -dtT test
