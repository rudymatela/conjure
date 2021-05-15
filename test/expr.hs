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

  , [false, true, zero, one] >$$< [notE, andE, orE, plus, times] == []
  , [notE, andE, orE, plus, times] >$$< [false, true, zero, one]
    == [ not' false
       , not' true
       , andE :$ false
       , andE :$ true
       , orE :$ false
       , orE :$ true
       , plus :$ zero
       , plus :$ one
       , times :$ zero
       , times :$ one
       ]
  , [notE, andE, orE, plus, times] >$$< [false, true, zero, one] >$$< [false, true, zero, one]
    == [ false -&&- false
       , false -&&- true
       , true -&&- false
       , true -&&- true
       , false -||- false
       , false -||- true
       , true -||- false
       , true -||- true
       , zero -+- zero
       , zero -+- one
       , one -+- zero
       , one -+- one
       , zero -*- zero
       , zero -*- one
       , one -*- zero
       , one -*- one
       ]

  , primitiveHoles [false, true]  ==  [b_]
  , primitiveHoles [zero, one]  ==  [i_]
  , primitiveHoles [false, zero]  ==  [b_, i_]

  , primitiveHoles [false, true, notE]  ==
      [ hole (undefined :: Bool)
      , hole (undefined :: Bool -> Bool)
      ]

  , primitiveHoles [false, true, notE, andE, orE]  ==
      [ hole (undefined :: Bool)
      , hole (undefined :: Bool -> Bool)
      , hole (undefined :: Bool -> Bool -> Bool)
      ]

  , primitiveHoles [false, true, andE, orE]  ==
      [ hole (undefined :: Bool)
      , hole (undefined :: Bool -> Bool)
      , hole (undefined :: Bool -> Bool -> Bool)
      ]

  , primitiveHoles [zero, one, plus, times]  ==
      [ hole (undefined :: Int)
      , hole (undefined :: Int -> Int)
      , hole (undefined :: Int -> Int -> Int)
      ]

  , primitiveHoles [false, true, andE, orE, zero, one, plus, times]
    == [ hole (undefined :: Bool)
       , hole (undefined :: Int)
       , hole (undefined :: Bool -> Bool)
       , hole (undefined :: Int -> Int)
       , hole (undefined :: Bool -> Bool -> Bool)
       , hole (undefined :: Int -> Int -> Int)
       ]

  , holds n $ \es -> all (`elem` primitiveHoles es) (map holeAsTypeOf es)
  , fails n $ \es -> primitiveHoles es == map holeAsTypeOf es

  , map show (primitiveApplications [false, true, andE, orE, zero, one, plus, times])
    == [ "False :: Bool"
       , "True :: Bool"
       , "(&&) :: Bool -> Bool -> Bool"
       , "(||) :: Bool -> Bool -> Bool"
       , "0 :: Int"
       , "1 :: Int"
       , "(+) :: Int -> Int -> Int"
       , "(*) :: Int -> Int -> Int"
       , "(_ &&) :: Bool -> Bool"
       , "(_ ||) :: Bool -> Bool"
       , "(_ +) :: Int -> Int"
       , "(_ *) :: Int -> Int"
       , "_ && _ :: Bool"
       , "_ || _ :: Bool"
       , "_ + _ :: Int"
       , "_ * _ :: Int"
       ]
  ]
