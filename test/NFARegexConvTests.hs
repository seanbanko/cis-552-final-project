module NFARegexConvTests where

import NFARegexConv
import FA
import NFADFAConv
import NFAOperations
import RegExp

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import Test.HUnit

-- Sipser 68 Example 1.56
r56 :: RegExp
r56 = RegExp.Star (alt (append (char 'a') (char 'b')) (char 'a'))

-- Expected output for r56
n56 :: NFA Int
n56 = F {
  states = Set.fromList [0,1,2,3,4,5,6,7], 
  alphabet = Set.fromList "ab", 
  transitionMap = Map.fromList [
      (0,Map.fromList [(FA.Char 'a',Set.fromList []),(FA.Char 'b',Set.fromList []),(Epsilon,Set.fromList [1])]),
      (1,Map.fromList [(FA.Char 'a',Set.fromList []),(FA.Char 'b',Set.fromList []),(Epsilon,Set.fromList [2,6])]),
      (2,Map.fromList [(FA.Char 'a',Set.fromList [3]),(FA.Char 'b',Set.fromList []),(Epsilon,Set.fromList [])]),
      (3,Map.fromList [(FA.Char 'a',Set.fromList []),(FA.Char 'b',Set.fromList []),(Epsilon,Set.fromList [4])]),
      (4,Map.fromList [(FA.Char 'a',Set.fromList []),(FA.Char 'b',Set.fromList [5]),(Epsilon,Set.fromList [])]),
      (5,Map.fromList [(FA.Char 'a',Set.fromList []),(FA.Char 'b',Set.fromList []),(Epsilon,Set.fromList [0])]),
      (6,Map.fromList [(FA.Char 'a',Set.fromList [7]),(FA.Char 'b',Set.fromList []),(Epsilon,Set.fromList [])]),
      (7,Map.fromList [(FA.Char 'a',Set.fromList []),(FA.Char 'b',Set.fromList []),(Epsilon,Set.fromList [0])])
  ],
  startState = 0, 
  acceptStates = Set.fromList [0,5,7]
}

test_toNFA :: Test
test_toNFA =
  "toNFA tests"
    ~: TestList
      [ toNFA r56 ~?= n56 ]

-- Tests taken from https://www.gatevidyalay.com/dfa-to-regular-expression-examples-automata/

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

-- Note: This is the expected output, not necessarily the simplified RegExp!
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

-- Note: This is the expected output, not necessarily the simplified RegExp!
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
d5RegExp = Alt
                (Append 
                    (Alt 
                        (Append 
                            (Alt 
                                (Append 
                                    (char '1') 
                                    (char '0')
                                ) 
                                (Append 
                                    (char '0') 
                                    (char '1') 
                                )
                            ) 
                            (Alt 
                                (char '1') 
                                (char '0')
                            )
                        ) 
                        (Append 
                            (Alt 
                                (Append 
                                  (char '1') 
                                  (char '1')
                                )
                                (Append 
                                  (char '0') 
                                  (char '0')
                                )
                            ) 
                            (Alt 
                                  (char '1') 
                                  (char '0')
                            )
                        )
                    ) 
                    (Star 
                        (Alt 
                            (char '1') 
                            (char '0')
                        )
                    )
                )
                (Alt 
                    (char '1') 
                    (char '0')
                )

test_convertTransitions :: Test
test_convertTransitions =
  "convert transitions tests"
    ~: TestList
      [ convertTransitions d5 6 7 ~?=
          Map.fromList [
              (0,Map.fromList [(0,Void),(1,RegExp.Char (Set.fromList "0")),(2,RegExp.Char (Set.fromList "1")),(3,Void),(4,Void),(5,Void)]),
              (1,Map.fromList [(0,Void),(1,Void),(2,Void),(3,RegExp.Char (Set.fromList "0")),(4,RegExp.Char (Set.fromList "1")),(5,Void),(7,Empty)]),
              (2,Map.fromList [(0,Void),(1,Void),(2,Void),(3,RegExp.Char (Set.fromList "1")),(4,RegExp.Char (Set.fromList "0")),(5,Void),(7,Empty)]),
              (3,Map.fromList [(0,Void),(1,Void),(2,Void),(3,Void),(4,Void),(5,Alt (RegExp.Char (Set.fromList "1")) (RegExp.Char (Set.fromList "0")))]),
              (4,Map.fromList [(0,Void),(1,Void),(2,Void),(3,Void),(4,Void),(5,Alt (RegExp.Char (Set.fromList "1")) (RegExp.Char (Set.fromList "0")))]),
              (5,Map.fromList [(0,Void),(1,Void),(2,Void),(3,Void),(4,Void),(5,Alt (RegExp.Char (Set.fromList "1")) (RegExp.Char (Set.fromList "0"))),(7,Empty)]),
              (6,Map.fromList [(0,Empty)])
          ]
      ]

test_convert :: Test
test_convert =
  "convert transitions tests"
    ~: TestList
      [ convert (toGNFA d1) ~?= d1RegExp,
        convert (toGNFA d2) ~?= d2RegExp,
        convert (toGNFA d5) ~?= d5RegExp
      ]