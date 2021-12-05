module FATests where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import FA
import Test.HUnit

-- nfa that accepts strings that contain either 101 or 11 as a substring (Sipser - page 48)
n1 :: NFA Int
n1 =
  let s = Set.fromList [1, 2, 3, 4]
      a = Set.fromList ['0', '1']
      state1Map = Map.fromList [(Char '0', Set.singleton 1), (Char '1', Set.fromList [1, 2]), (Epsilon, Set.empty)]
      state2Map = Map.fromList [(Char '0', Set.singleton 3), (Char '1', Set.empty), (Epsilon, Set.singleton 3)]
      state3Map = Map.fromList [(Char '0', Set.empty), (Char '1', Set.singleton 4), (Epsilon, Set.empty)]
      state4Map = Map.fromList [(Char '0', Set.singleton 4), (Char '1', Set.singleton 4), (Epsilon, Set.empty)]
      tm = Map.fromList [(1, state1Map), (2, state2Map), (3, state3Map), (4, state4Map)]
      ss = 1
      as = Set.singleton 4
   in F s a tm ss as

-- nfa that accepts strings containing a 1 in the third position from the end (Sipser - page 51)
n2 :: NFA Int
n2 =
  let s = Set.fromList [1, 2, 3, 4]
      a = Set.fromList ['0', '1']
      state1Map = Map.fromList [(Char '0', Set.singleton 1), (Char '1', Set.fromList [1, 2]), (Epsilon, Set.empty)]
      state2Map = Map.fromList [(Char '0', Set.singleton 3), (Char '1', Set.singleton 3), (Epsilon, Set.empty)]
      state3Map = Map.fromList [(Char '0', Set.singleton 4), (Char '1', Set.singleton 4), (Epsilon, Set.empty)]
      state4Map = Map.fromList [(Char '0', Set.empty), (Char '1', Set.empty), (Epsilon, Set.empty)]
      tm = Map.fromList [(1, state1Map), (2, state2Map), (3, state3Map), (4, state4Map)]
      ss = 1
      as = Set.singleton 4
   in F s a tm ss as

-- nfa that accepts strings of the form 0^k where k is a multiple of 2 or 3 (Sipser - page 52)
n3 :: NFA Int
n3 =
  let s = Set.fromList [1, 2, 3, 4, 5, 6]
      a = Set.fromList ['0']
      state1Map = Map.fromList [(Char '0', Set.empty), (Epsilon, Set.fromList [2, 3])]
      state2Map = Map.fromList [(Char '0', Set.singleton 4), (Epsilon, Set.empty)]
      state3Map = Map.fromList [(Char '0', Set.singleton 5), (Epsilon, Set.empty)]
      state4Map = Map.fromList [(Char '0', Set.singleton 2), (Epsilon, Set.empty)]
      state5Map = Map.fromList [(Char '0', Set.singleton 6), (Epsilon, Set.empty)]
      state6Map = Map.fromList [(Char '0', Set.singleton 3), (Epsilon, Set.empty)]
      tm = Map.fromList [(1, state1Map), (2, state2Map), (3, state3Map), (4, state4Map), (5, state5Map), (6, state6Map)]
      ss = 1
      as = Set.fromList [2, 3]
   in F s a tm ss as

-- nfa (Sipser - page 53)
n4 :: NFA Int
n4 =
  let s = Set.fromList [1, 2, 3]
      a = Set.fromList ['a', 'b']
      state1Map = Map.fromList [(Char 'a', Set.empty), (Char 'b', Set.singleton 2), (Epsilon, Set.singleton 3)]
      state2Map = Map.fromList [(Char 'a', Set.fromList [2, 3]), (Char 'b', Set.singleton 3), (Epsilon, Set.empty)]
      state3Map = Map.fromList [(Char 'a', Set.singleton 1), (Char 'b', Set.empty), (Epsilon, Set.empty)]
      tm = Map.fromList [(1, state1Map), (2, state2Map), (3, state3Map)]
      ss = 1
      as = Set.singleton 1
   in F s a tm ss as

d5 :: DFA String
d5 =
  let s = Set.fromList ["q0", "q1", "q2", "q3", "q4", "q5"]
      a = Set.fromList ['0', '1']
      q0Map = Map.fromList [('0', "q1"), ('1', "q2")]
      q1Map = Map.fromList [('0', "q3"), ('1', "q4")]
      q2Map = Map.fromList [('0', "q4"), ('1', "q3")]
      q3Map = Map.fromList [('0', "q5"), ('1', "q5")]
      q4Map = Map.fromList [('0', "q5"), ('1', "q5")]
      q5Map = Map.fromList [('0', "q5"), ('1', "q5")]
      tm = Map.fromList [("q0", q0Map), ("q1", q1Map), ("q2", q2Map), ("q3", q3Map), ("q4", q4Map), ("q5", q5Map)]
      ss = "q0"
      as = Set.fromList ["q1", "q2", "q5"]
   in F s a tm ss as

d6 :: DFA String
d6 =
  let s = Set.fromList ["a", "b", "c", "d", "e", "f"]
      a = Set.fromList ['0', '1']
      aMap = Map.fromList [('0', "b"), ('1', "c")]
      bMap = Map.fromList [('0', "a"), ('1', "d")]
      cMap = Map.fromList [('0', "e"), ('1', "f")]
      dMap = Map.fromList [('0', "e"), ('1', "f")]
      eMap = Map.fromList [('0', "e"), ('1', "f")]
      fMap = Map.fromList [('0', "f"), ('1', "f")]
      tm = Map.fromList [("a", aMap), ("b", bMap), ("c", cMap), ("d", dMap), ("e", eMap), ("f", fMap)]
      ss = "a"
      as = Set.fromList ["c", "d", "e"]
   in F s a tm ss as

d7 :: DFA String
d7 =
  let s = Set.fromList ["a", "b", "c", "d", "e"]
      a = Set.fromList ['a', 'b']
      aMap = Map.fromList [('a', "b"), ('b', "d")]
      bMap = Map.fromList [('a', "c"), ('b', "e")]
      cMap = Map.fromList [('a', "b"), ('b', "e")]
      dMap = Map.fromList [('a', "c"), ('b', "e")]
      eMap = Map.fromList [('a', "e"), ('b', "e")]
      tm = Map.fromList [("a", aMap), ("b", bMap), ("c", cMap), ("d", dMap), ("e", eMap)]
      ss = "a"
      as = Set.fromList ["c", "e"]
   in F s a tm ss as

test_stringTransitionN :: Test
test_stringTransitionN =
  "string transition tests"
    ~: TestList
      [ stringTransitionN n3 (startState n3) "" ~?= Set.fromList [1, 2, 3],
        stringTransitionN n3 (startState n3) "0" ~?= Set.fromList [4, 5],
        stringTransitionN n3 (startState n3) "00" ~?= Set.fromList [2, 6],
        stringTransitionN n3 (startState n3) "000" ~?= Set.fromList [3, 4],
        stringTransitionN n3 5 "" ~?= Set.singleton 5,
        stringTransitionN n3 3 "00" ~?= Set.singleton 6
      ]

test_acceptN :: Test
test_acceptN =
  "accept tests"
    ~: TestList
      [ acceptN n1 "" ~?= False,
        acceptN n1 "1" ~?= False,
        acceptN n1 "00" ~?= False,
        acceptN n1 "101" ~?= True,
        acceptN n1 "11" ~?= True,
        acceptN n1 "01110" ~?= True,
        acceptN n1 "101110" ~?= True,
        acceptN n1 "10001" ~?= False,
        acceptN n1 "001001" ~?= False,
        acceptN n2 "" ~?= False,
        acceptN n2 "1" ~?= False,
        acceptN n2 "01" ~?= False,
        acceptN n2 "101" ~?= True,
        acceptN n2 "0110101100" ~?= True,
        acceptN n2 "0101010101001" ~?= False,
        acceptN n2 "101010101" ~?= True,
        acceptN n3 "" ~?= True,
        acceptN n3 "0" ~?= False,
        acceptN n3 "00" ~?= True,
        acceptN n3 "000" ~?= True,
        acceptN n3 "0000" ~?= True,
        acceptN n3 "00000" ~?= False,
        acceptN n3 "000000" ~?= True,
        acceptN n3 "0000000" ~?= False,
        acceptN n3 "00000000" ~?= True,
        acceptN n3 "000000000" ~?= True,
        acceptN n3 "0000000000" ~?= True
      ]
