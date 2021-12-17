module NFARegexConvTests where

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import FA
import Generators
import NFADFAConv
import NFAOperations
import NFARegexConv
import RegExp
import Test.HUnit
import Test.QuickCheck

{-
  RegExp to NFA Conversion Tests
  Reference: Sipser
-}

-- Sipser 68 Example 1.56
r56 :: RegExp
r56 = RegExp.Star (alt (append (char 'a') (char 'b')) (char 'a'))

-- Expected output for r56
n56 :: NFA Int
n56 =
  F
    { states = Set.fromList [0, 1, 2, 3, 4, 5, 6, 7],
      alphabet = Set.fromList "ab",
      transitionMap =
        Map.fromList
          [ (0, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [1])]),
            (1, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [2, 6])]),
            (2, Map.fromList [(FA.Char 'a', Set.fromList [3]), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [])]),
            (3, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [4])]),
            (4, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList [5]), (Epsilon, Set.fromList [])]),
            (5, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [1])]),
            (6, Map.fromList [(FA.Char 'a', Set.fromList [7]), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [])]),
            (7, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [1])])
          ],
      startState = 0,
      acceptStates = Set.fromList [0, 5, 7]
    }

-- Sipser 68 Example 1.58
r58 :: RegExp
r58 = append (RegExp.Star (alt (char 'a') (char 'b'))) (append (char 'a') (append (char 'b') (char 'a')))

-- Expected output for r58
n58 :: NFA Int
n58 =
  F
    { states = Set.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11],
      alphabet = Set.fromList "ab",
      transitionMap =
        Map.fromList
          [ (0, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [1, 6])]),
            (1, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [2, 4])]),
            (2, Map.fromList [(FA.Char 'a', Set.fromList [3]), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [])]),
            (3, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [1, 6])]),
            (4, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList [5]), (Epsilon, Set.fromList [])]),
            (5, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [1, 6])]),
            (6, Map.fromList [(FA.Char 'a', Set.fromList [7]), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [])]),
            (7, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [8])]),
            (8, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList [9]), (Epsilon, Set.fromList [])]),
            (9, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [10])]),
            (10, Map.fromList [(FA.Char 'a', Set.fromList [11]), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [])]),
            (11, Map.fromList [(FA.Char 'a', Set.fromList []), (FA.Char 'b', Set.fromList []), (Epsilon, Set.fromList [])])
          ],
      startState = 0,
      acceptStates = Set.fromList [11]
    }

test_toNFA :: Test
test_toNFA =
  "toNFA tests"
    ~: TestList
      [ toNFA r56 ~?= n56,
        toNFA r58 ~?= n58
      ]

{-
  NFA/DFA to RegExp Conversion Tests
  Tests examples from https://www.gatevidyalay.com/dfa-to-regular-expression-examples-automata/
  Note: The expected outputs are not necessarily the simplified regular expressions
-}

d1 :: DFA Int
d1 =
  let qs = Set.fromList [0, 1]
      sigma = Set.fromList ['0', '1']
      q0Map = Map.fromList [('0', 1)]
      q1Map = Map.fromList [('1', 0)]
      tm = Map.fromList [(0, q0Map), (1, q1Map)]
      q0 = 0
      fs = Set.singleton 1
   in F qs sigma tm q0 fs

d1RegExp :: RegExp
d1RegExp = append (char '0') (RegExp.star (append (char '1') (char '0')))

d2 :: DFA Int
d2 =
  let qs = Set.fromList [1, 2, 3, 4, 5]
      sigma = Set.fromList ['a', 'b', 'c', 'd']
      q1Map = Map.fromList [('a', 2)]
      q2Map = Map.fromList [('b', 4), ('c', 3), ('d', 5)]
      q3Map = Map.empty
      q4Map = Map.empty
      q5Map = Map.empty
      tm = Map.fromList [(1, q1Map), (2, q2Map), (3, q3Map), (4, q4Map), (5, q5Map)]
      q0 = 1
      fs = Set.fromList [3, 4, 5]
   in F qs sigma tm q0 fs

d2RegExp :: RegExp
d2RegExp = alt (append (char 'a') (char 'd')) (alt (append (char 'a') (char 'b')) (append (char 'a') (char 'c')))

d2RegExpSimplified :: RegExp
d2RegExpSimplified = append (char 'a') (alt (char 'b') (alt (char 'c') (char 'd')))

d5 :: DFA Int
d5 =
  let s = Set.fromList [0, 1, 2, 3, 4, 5]
      a = Set.fromList ['0', '1']
      q0Map = Map.fromList [('0', 1), ('1', 2)]
      q1Map = Map.fromList [('0', 3), ('1', 4)]
      q2Map = Map.fromList [('0', 4), ('1', 3)]
      q3Map = Map.fromList [('0', 5), ('1', 5)]
      q4Map = Map.fromList [('0', 5), ('1', 5)]
      q5Map = Map.fromList [('0', 5), ('1', 5)]
      tm = Map.fromList [(0, q0Map), (1, q1Map), (2, q2Map), (3, q3Map), (4, q4Map), (5, q5Map)]
      ss = 0
      as = Set.fromList [1, 2, 5]
   in F s a tm ss as

d5RegExp :: RegExp
d5RegExp =
  Alt
    ( Append
        ( Alt
            ( Append
                ( Alt
                    (Append (char '1') (char '0'))
                    (Append (char '0') (char '1'))
                )
                ( Alt
                    (char '1')
                    (char '0')
                )
            )
            ( Append
                ( Alt
                    (Append (char '1') (char '1'))
                    (Append (char '0') (char '0'))
                )
                ( Alt
                    (char '1')
                    (char '0')
                )
            )
        )
        ( Star
            ( Alt
                (char '1')
                (char '0')
            )
        )
    )
    ( Alt
        (char '1')
        (char '0')
    )

test_convert :: Test
test_convert =
  "convert tests"
    ~: TestList
      [ convert (toGNFA d1) ~?= d1RegExp,
        convert (toGNFA d2) ~?= d2RegExp,
        convert (toGNFA d5) ~?= d5RegExp
      ]

prop_roundTripR :: RegExp -> Property
prop_roundTripR r = toRegExp (toNFA r) %==% r

-- not necessarily equal, generate the same language

prop_roundTripN :: NFA Int -> Bool
prop_roundTripN nfa =
  let nfa' = toNFA (toRegExp nfa)
   in equivalentDFA (toDFA nfa) (toDFA nfa')

-- not necessarily equal, accept the same language

-- if you convert a regexp to a nfa, then a string generated by the regexp should be accepted by the nfa
prop_acceptRegExp :: RegExp -> Bool
prop_acceptRegExp = undefined
