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
  eg/factorial \
  eg/fibonacci \
  eg/fib01 \
  eg/id \
  eg/ints \
  eg/bools \
  eg/gcd \
  eg/list \
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
  test/utils

all: mk/toplibs

all-all: all $(EG) $(TESTS)

test: $(TESTS) $(patsubst %,%.run,$(TESTS)) diff-test test-sdist

.PHONY: bench
bench: $(EG) $(patsubst %,%.bench,$(EG))
	@mkdir -p bench/runtime/$$HOSTNAME
	./bench/versions | tee bench/runtime/$$HOSTNAME/versions

ghci: src/Conjure.ghci

%.bench: %
	@mkdir -p bench/runtime/$$HOSTNAME/`dirname $<`
	@printf "%-18s " $<
	@/usr/bin/time -f%e ./$< 2>&1 >/dev/null | \
	python3 -c 'print("%.1f" % float(input()))' | \
	tee bench/runtime/$$HOSTNAME/$<.runtime

diff-test: $(EG) $(patsubst %,%.diff-test,$(EG))

out: $(EG) $(patsubst %,%.out,$(EG))

test-sdist:
	./test/sdist

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

%.out: %
	./$< >$<.out

%.diff-test: %
	./$< | diff -rud $<.out -

# lists files missing copyright notices
list-missing-copyright:
	grep -LRE '(Copyright|\(C\))' `git ls-tree -r master --name-only | grep -vE '(\.(runtime|out)|versions|toplibs|(Toplibs|All)\.hs|depend.mk)$$'` || true

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


include mk/haskell.mk
