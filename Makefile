export IDRIS2 ?= idris2

RUNTESTS := build/exec/runtests

MKDIR := mkdir -p
LN := ln

.PHONY: all coop clean

all: coop

coop:
	${IDRIS2} --build coop.ipkg

clean:
	${IDRIS2} --clean coop.ipkg
	${RM} -r build
	@
	${MAKE} -C tests -f tests.mk clean

.PHONY: test test-coop

test: test-coop

test-coop: coop
	${MAKE} -C tests -f tests.mk only="${only}"
