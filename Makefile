GHCIMPORTDIRS = src:test
GHCFLAGS = -O2 -v0 \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
HADDOCKFLAGS = \
  -i $(shell find ~/.cabal -name leancheck.haddock | tail -1) \
  -i $(shell find ~/.cabal -name express.haddock   | tail -1) \
  -i $(shell find ~/.cabal -name speculate.haddock | tail -1) \
  -i $(shell find /usr/share/doc/ghc/html/libraries -name template-haskell.haddock | tail -1) \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo --optghc=-dynamic) \
  | grep -v "^Warning: Couldn't find .haddock for export [A-Z]$$"
INSTALL_DEPS = leancheck express speculate template-haskell

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
  eg/pow \
  eg/replicate \
  eg/setelem \
  eg/sort \
  eg/subset \
  eg/spec \
  eg/tapps \
  eg/tree \
  eg/bst \
  bench/candidates \
  bench/redundants \
  bench/erroneous \
  bench/ill-hit \
  bench/longshot \
  bench/lowtests \
  bench/self \
  bench/take-drop \
  bench/p12 \
  bench/p30 \
  bench/gps \
  bench/gps2 \
  bench/terpret \
  bench/weird \
  proto/u-conjure

TESTS = \
  test/expr \
  test/defn \
  test/conjurable \
  test/derive \
  test/utils

all: mk/toplibs

all-all: all $(EG) $(TESTS)

test: test-makefile $(TESTS) $(patsubst %,%.run,$(TESTS)) diff-test test-sdist

ghci: src/Conjure.ghci

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

clean: clean-hi-o clean-haddock
	rm -f $(EG) $(TESTS) mk/toplibs

full-clean: clean clean-cabal clean-stack
	rm -f tags TAGS mk/toplibs

%.run: %
	./$<

%.txt: %
	./$< >$<.txt

%.diff: %
	./$< | diff -rud $<.txt -

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
