# Makefile for Conjure
#
# Copyright (C) 2021-2025 Rudy Matela
# Distributed under a 3-Clause BSD license, see the LICENSE file

GHCIMPORTDIRS = src:test
GHCFLAGS = -O2 -v0 \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
# -Wall -Werror -Wno-unused-matches -Wno-orphans -Wno-name-shadowing -Wno-incomplete-uni-patterns -Wno-unused-pattern-binds
HADDOCKFLAGS = \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo --optghc=-dynamic) \
  | grep -v "^Warning: Couldn't find .haddock for export [A-Z]$$"
INSTALL_DEPS = leancheck express speculate template-haskell

NJOBS := $(shell grep ^processor /proc/cpuinfo | head -n -1 | wc -l | sed 's/^0$$/1/')
LONG := $(shell which long >/dev/null 2>/dev/null && echo long)

EG = \
  eg/arith \
  eg/count \
  eg/dupos \
  eg/either \
  eg/factorial \
  eg/fibonacci \
  eg/fib01 \
  eg/higher \
  eg/id \
  eg/ints \
  eg/bools \
  eg/gcd \
  eg/list \
  eg/maybe \
  eg/oddeven \
  eg/pow \
  eg/replicate \
  eg/setelem \
  eg/take-drop \
  eg/sort \
  eg/subset \
  eg/spec \
  eg/tapps \
  eg/tree \
  eg/bst \
  eg/tri \
  eg/bits \
  eg/digits \
  eg/peano \
  eg/these \
  eg/tuple \
  eg/conditionals \
  bench/candidates \
  bench/redundants \
  bench/erroneous \
  bench/ill-hit \
  bench/longshot \
  bench/lowtests \
  bench/strategies \
  bench/self \
  bench/carry-on \
  bench/i12 \
  bench/i30 \
  bench/gps \
  bench/psb2 \
  bench/terpret \
  bench/unique \
  bench/weird \
  proto/u-conjure

TESTS = \
  test/factorial \
  test/expr \
  test/defn \
  test/conjurable \
  test/derive \
  test/red \
  test/utils

RMTIME = sed 's/^-- [0-9.]*s, /-- /'

all: mk/toplibs

all-all: all $(EG) $(TESTS)

test: test-makefile $(TESTS) $(patsubst %,%.run,$(TESTS)) diff-test test-sdist

ghci: src/Conjure.ghci

.PHONY: tags
tags:
	ctags -R src

# Disclaimer: This bench target is not intended to generate paper-grade runtime
#             datapoints as it runs each benchmark just once.  This target is
#             meant to track large runtime changes across different git
#             versions.
.PHONY: bench
bench: $(EG) $(patsubst %,%.bench,$(EG))
	@mkdir -p bench/runtime/$$HOSTNAME
	./bench/versions | tee bench/runtime/$$HOSTNAME/versions

%.bench: %
	@mkdir -p bench/runtime/$$HOSTNAME/`dirname $<`
	@printf "%-18s " $<
	@/usr/bin/time -f%e ./$< 2>&1 >/dev/null | \
	python3 -c 'print("%.1f" % float(input()))' | \
	tee bench/runtime/$$HOSTNAME/$<.runtime

diff-test: $(EG) $(patsubst %,%.diff,$(EG))

out: txt

txt: $(EG) $(patsubst %,%.txt,$(EG))

test-sdist:
	./test/sdist

test-makefile: test/mk.run

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test expr

test-via-stack:
	stack test code-conjure:test:expr --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

test-on-ghc-9.10:
	make test GHC=ghc-9.10 GHCIMPORTDIRS=src:test:../leancheck/src:../express/src:../speculate/src

fastest:
	$(LONG) make test -j$(NJOBS)

fastestbench:
	$(LONG) make test -j$(NJOBS) && $(LONG) make bench

fastxtestbench:
	$(LONG) make txt -j$(NJOBS) && $(LONG) make test -j$(NJOBS) && $(LONG) make bench

clean: clean-hi-o clean-haddock
	rm -f $(EG) $(TESTS) mk/toplibs */.*.txt

full-clean: clean clean-cabal clean-stack
	rm -f tags TAGS mk/toplibs

%.run: %
	./$<

.PRECIOUS: .%.txt  # prevent deletion by make

.%.txt: %
	./$< >$@

%.txt: .%.txt
	$(RMTIME) $< >$@

%.tee: %
	$(LONG) unbuffer ./$< | $(RMTIME) -u | tee $<.txt

%.diff: %
	./$< | $(RMTIME) | diff -rud $<.txt -

# lists files missing copyright notices
list-missing-copyright:
	grep -LRE '(Copyright|\(C\))' `git ls-tree -r master --name-only | grep -vE '(\.(runtime|txt)|versions|toplibs|(Toplibs|All)\.hs|depend.mk)$$'` || true

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and test programs so long as they don't share dependencies _not_ stored
# in src/ and test/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

p12: bench/p12
	./bench/bench $< | tee bench/runtime/$$HOSTNAME/p12.runtimes

p30: bench/p30
	./bench/bench $< | tee bench/runtime/$$HOSTNAME/p30.runtimes

avgs:
	runhaskell bench/avgs.hs <bench/runtime/$$HOSTNAME/p12.runtimes
	runhaskell bench/avgs.hs <bench/runtime/$$HOSTNAME/p30.runtimes

gps-each: bench/gps
	for i in {1..29}; do ./bench/time ./bench/gps $$i; done

terpret-each: bench/terpret
	for i in {1..8}; do ./bench/time ./bench/terpret $$i; done

gps2-each: bench/gps2
	for i in {1..25}; do ./bench/time ./bench/gps2 $$i; done

ls-eg:
	@for eg in $(EG); do echo $$eg; done

ls-test:
	@for test in $(TESTS); do echo $$test; done

include mk/haskell.mk
