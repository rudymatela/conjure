GHCIMPORTDIRS = src:test
GHCFLAGS = -O2 \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
HADDOCKFLAGS = \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo --optghc=-dynamic)
INSTALL_DEPS = leancheck express speculate

EG = \
  eg/arith \
  eg/count \
  eg/factorial \
  eg/fibonacci \
  eg/ints \
  eg/bools \
  eg/list \
  eg/setelem \
  eg/spec \
  eg/tapps \
  eg/tree \
  bench/ill-hit \
  bench/longshot \
  bench/self \
  bench/take-drop \
  proto/u-conjure

TESTS = \
  test/expr \
  test/conjurable \
  test/utils

all: mk/toplibs

all-all: all $(EG) $(TESTS)

test: $(patsubst %,%.run,$(TESTS)) diff-test test-sdist

.PHONY: bench
bench: $(patsubst %,%.bench,$(EG))
	@mkdir -p bench/runtime/$$HOSTNAME
	./bench/versions | tee bench/runtime/$$HOSTNAME/versions

ghci: src/Conjure.ghci

%.bench: %
	@mkdir -p bench/runtime/$$HOSTNAME/`dirname $<`
	@printf "%-18s " $<
	@/usr/bin/time -f%e ./$< 2>&1 >/dev/null | \
	python3 -c 'print("%.1f" % float(input()))' | \
	tee bench/runtime/$$HOSTNAME/$<.runtime

diff-test: $(patsubst %,%.diff-test,$(EG))

out: $(patsubst %,%.out,$(EG))

test-sdist:
	./test/sdist

test-via-cabal:
	cabal configure --enable-tests --enable-benchmarks --ghc-options="$(GHCFLAGS) -O0"
	cabal build
	cabal test expr

test-via-stack:
	stack test code-conjure:test:expr --ghc-options="$(GHCFLAGS) -O0" --system-ghc --no-install-ghc --no-terminal

hugs-test:
	echo 'Unsupported'

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

include mk/haskell.mk
