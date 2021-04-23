GHCIMPORTDIRS = src:test
GHCFLAGS = -O2 \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo -dynamic)
HADDOCKFLAGS = --no-print-missing-docs \
  $(shell grep -q "Arch Linux" /etc/lsb-release && echo --optghc=-dynamic)

EG = \
  bench/arith \
  bench/factorial \
  bench/ints \
  bench/bools \
  proto/u-conjure

TESTS = \
  test/expr

all: mk/toplibs

all-all: all $(EG) $(TESTS)

test: $(patsubst %,%.run,$(TESTS)) diff-test

.PHONY: bench
bench: $(patsubst %,%.bench,$(EG))
	@mkdir -p bench/runtime/$$HOSTNAME
	./bench/versions | tee bench/runtime/$$HOSTNAME/versions

%.bench: %
	@mkdir -p bench/runtime/$$HOSTNAME/`dirname $<`
	@printf "%-18s " $<
	@/usr/bin/time -f%e ./$< 2>&1 >/dev/null | tee bench/runtime/$$HOSTNAME/$<.runtime

diff-test: $(patsubst %,%.diff-test,$(EG))

update-diff-test: $(patsubst %,%.update-diff-test,$(EG))

test-sdist:
	./test/sdist

clean: clean-hi-o clean-haddock
	rm -f $(EG) $(TESTS)

full-clean: clean clean-cabal clean-stack
	rm -f tags TAGS

%.run: %
	./$<

%.diff-test: %
	./$< | diff -rud test/model/$<.out -

%.update-diff-test: %
	./$< >           test/model/$<.out

# lists files missing copyright notices
list-missing-copyright:
	grep -LR Copyright `git ls-tree -r master --name-only | grep -vE '(\.(runtime|out)|versions|toplibs|(Toplibs|All)\.hs|depend.mk)$$'` || true

# NOTE: (very hacky!) the following target allows parallel compilation (-jN) of
# eg and test programs so long as they don't share dependencies _not_ stored
# in src/ and test/.  Runnable binaries should depend on mk/toplibs instead of
# actual Haskell source files
mk/toplibs: mk/Toplibs.o
	touch mk/toplibs

include mk/haskell.mk
