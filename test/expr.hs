-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).

import Test

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , recursexpr 6 (ff xx) (ff xx)
                      == (ff xx)

  , recursexpr 6 (ff xx) (one -+- ff xx)
                      == (one -+- (one -+- (one -+- (one -+- ff xx))))

  , recursexpr 6 (ff xx) (if' pp one (xx -*- ff xx))
                      == (if' pp one (xx -*- (if' pp one (xx -*- ff xx))))

  , recursexpr 6 (ff xx) (if' pp one (xx -*- ff (gg xx)))
                      == (if' pp one (xx -*- (if' pp one (gg xx -*- ff (gg (gg xx))))))

  , mayNotEvaluateArgument (plus :$ xx)       == False
  , mayNotEvaluateArgument (andE :$ pp)       == True
  , mayNotEvaluateArgument (orE :$ qq)        == True

  , mayNotEvaluateArgument (if' pp xx yy)     == False
  , mayNotEvaluateArgument (andE :$ pp :$ qq) == False
  , mayNotEvaluateArgument (orE  :$ pp :$ qq) == False

  , apparentlyTerminates ffE (ff xx) == False
  , apparentlyTerminates ffE (if' pp zero (ff xx)) == True
  , apparentlyTerminates ffE (if' pp (ff xx) zero) == False
  , apparentlyTerminates ffE (if' (odd' (ff xx)) zero zero) == False
  ]
