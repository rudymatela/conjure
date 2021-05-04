bench/ill-hit: \
  bench/ill-hit.hs \
  mk/toplibs
bench/ill-hit.o: \
  bench/ill-hit.hs
bench/self: \
  bench/self.hs \
  mk/toplibs
bench/self.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs \
  bench/self.hs
eg/arith: \
  eg/arith.hs \
  mk/toplibs
eg/arith.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs \
  eg/arith.hs
eg/bools: \
  eg/bools.hs \
  mk/toplibs
eg/bools.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs \
  eg/bools.hs
eg/factorial: \
  eg/factorial.hs \
  mk/toplibs
eg/factorial.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs \
  eg/factorial.hs
eg/ints: \
  eg/ints.hs \
  mk/toplibs
eg/ints.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs \
  eg/ints.hs
eg/list: \
  eg/list.hs \
  mk/toplibs
eg/list.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs \
  eg/list.hs
eg/tapps: \
  eg/tapps.hs \
  mk/toplibs
eg/tapps.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs \
  eg/tapps.hs
mk/All.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs \
  mk/Toplibs.hs
proto/u-conjure.o: \
  proto/u-conjure.hs
proto/u-conjure: \
  proto/u-conjure.hs \
  mk/toplibs
src/Conjure/Conjurable.o: \
  src/Conjure/Utils.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Conjurable.hs
src/Conjure/Engine.o: \
  src/Conjure/Utils.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs
src/Conjure/Expr.o: \
  src/Conjure/Utils.hs \
  src/Conjure/Expr.hs
src/Conjure.o: \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs
src/Conjure/Utils.o: \
  src/Conjure/Utils.hs
test/conjurable.o: \
  test/Test.hs \
  test/conjurable.hs \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs
test/conjurable: \
  test/Test.hs \
  test/conjurable.hs \
  mk/toplibs
test/expr.o: \
  test/Test.hs \
  test/expr.hs \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs
test/expr: \
  test/Test.hs \
  test/expr.hs \
  mk/toplibs
test/Test.o: \
  test/Test.hs \
  src/Conjure/Utils.hs \
  src/Conjure.hs \
  src/Conjure/Expr.hs \
  src/Conjure/Engine.hs \
  src/Conjure/Conjurable.hs
test/Test: \
  test/Test.hs \
  mk/toplibs
