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

  , holds n $ \e -> sort (valuesBFS e) == sort (values e)
  , holds n $ \e -> holesBFS e == filter isHole (valuesBFS e)
  , valuesBFS false == [false]
  , valuesBFS true  == [true]
  , valuesBFS zero  == [zero]
  , valuesBFS one   == [one]
  , valuesBFS (not' false) == [notE, false]
  , valuesBFS (not' true)  == [notE, true]
  , valuesBFS (not' $ not' true) == [notE, notE, true]
  , valuesBFS (false -&&- true) == [true, andE, false]
  , valuesBFS (true -||- false) == [false, orE, true]
  , valuesBFS (one -*- two -+- three -*- xx)
    == [plus, xx, two, times, three, times, one]
    -- (((+) (((*) 1) 2)) (((*) 3) x))

  , fillBFS (b_ -&&- b_) false == (b_ -&&- false)
  , fillBFS (b_ -&&- b_) (b_ -&&- b_) == (b_ -&&- (b_ -&&- b_))
  , fillBFS (b_ -&&- (b_ -&&- b_)) false == (false -&&- (b_ -&&- b_))
  , fillBFS (b_ -&&- (b_ -&&- b_)) (b_ -&&- b_) == ((b_ -&&- b_) -&&- (b_ -&&- b_))
  , fillBFS ((b_ -&&- b_) -&&- (b_ -&&- b_)) false == ((b_ -&&- b_) -&&- (b_ -&&- false))
  , fillBFS true false == true

  , holds n $ \(SameTypeE e1 e2) -> let e3 = fillBFS e1 e2
                                    in e3 == e1
                                    || length (holes e3) == length (holes e1) - 1 + length (holes e2)

  , possibleHoles [plus, times, zero, one]
    == [ hole (undefined :: Int)
       , hole (undefined :: Int -> Int)
       , hole (undefined :: Int -> Int -> Int)
       ]

  , possibleHoles [andE, orE, false, true]
    == [ hole (undefined :: Bool)
       , hole (undefined :: Bool -> Bool)
       , hole (undefined :: Bool -> Bool -> Bool)
       ]

  , possibleHoles [plus, times, zero, one, andE, orE, false, true]
    == [ hole (undefined :: Bool)
       , hole (undefined :: Int)
       , hole (undefined :: Bool -> Bool)
       , hole (undefined :: Int -> Int)
       , hole (undefined :: Bool -> Bool -> Bool)
       , hole (undefined :: Int -> Int -> Int)
       ]

  , possibleHoles [plus, times, zero, one, value "odd" (odd :: Int -> Bool)]
    == [ hole (undefined :: Bool)
       , hole (undefined :: Int)
       , hole (undefined :: Int -> Bool)
       , hole (undefined :: Int -> Int)
       , hole (undefined :: Int -> Int -> Int)
       ]

  , useMatches [xx,yy] [xx,yy] == [[(xx,xx), (yy,yy)]]
  , useMatches [xx,yy] [yy,xx] == [[(xx,xx), (yy,yy)]]
  , useMatches [yy,xx] [xx,yy] == [[(yy,yy), (xx,xx)]]
  , useMatches [xx,yy] [xx,xx] == []

  , useMatches [xx,yy] [abs' xx, abs' yy]
    == [ [ (xx, abs' xx)
         , (yy, abs' yy)
         ]
       ]

  , useMatches [xx-:-xxs, yy-:-yys] [abs' xx, abs' yy]
    == [ [ (xx-:-xxs, abs' xx)
         , (yy-:-yys, abs' yy)
         ]
       ]

  , useMatches [xx-:-xxs, yy-:-yys] [xx-:-xxs, yy-:-yys]
    == [ [ (xx-:-xxs, xx-:-xxs)
         , (yy-:-yys, yy-:-yys)
         ]
       ]

  , useMatches [xx-:-xxs, yy-:-yys] [yy-:-xxs, yy-:-yys]
    == [ [ (xx-:-xxs, yy-:-xxs)
         , (yy-:-yys, yy-:-yys)
         ]
       ]

  , useMatches [xx-:-xxs, yy-:-yys] [yy-:-xxs, xx-:-yys]
    == [ [ (xx-:-xxs, yy-:-xxs)
         , (yy-:-yys, xx-:-yys)
         ]
       , [ (xx-:-xxs, xx-:-yys)
         , (yy-:-yys, yy-:-xxs)
         ]
       ]

  -- type-correct applications are allowed on $$, $$|< and $$**
  , absE $$   zero  ==  Just (abs' zero)
  , absE $$|< zero  ==  Just (abs' zero)
  , absE $$** zero  ==  Just (abs' zero)

  -- kind-correct applications are allowed on $$|< and $$**
  , ordE $$   zero  ==  Nothing
  , ordE $$|< zero  ==  Just (ordE :$ zero)
  , ordE $$** zero  ==  Just (ordE :$ zero)

  -- type-incorrect applications are allowed on $$**
  , zero $$   zero  ==  Nothing
  , zero $$|< zero  ==  Nothing
  , zero $$** zero  ==  Just (zero :$ zero)

  , conflicts (one -+- two) (three -+- four) == [(one,three), (two,four)]
  , conflicts (xx -:- nil) (xx -:- yy -:- yys) == [(nil, yy -:- yys)]
  , conflicts (one -:- one -:- nil) (zero -:- zero -:- xx -:- xxs)
    == [(one,zero), (nil,xx -:- xxs)]
  ]
