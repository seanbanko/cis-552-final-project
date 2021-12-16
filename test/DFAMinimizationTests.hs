module DFAMinimizationTests where

import DFAMinimization
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import FA
import FATests
import Generators
import NFADFAConv
import Test.HUnit
import Test.QuickCheck

d5' :: DFA (Set String)
d5' =
  let s = Set.fromList [Set.singleton "q0", Set.fromList ["q1", "q2"], Set.fromList ["q3", "q4"], Set.singleton "q5"]
      a = Set.fromList ['0', '1']
      q0Map = Map.fromList [('0', Set.fromList ["q1", "q2"]), ('1', Set.fromList ["q1", "q2"])]
      q12Map = Map.fromList [('0', Set.fromList ["q3", "q4"]), ('1', Set.fromList ["q3", "q4"])]
      q34Map = Map.fromList [('0', Set.singleton "q5"), ('1', Set.singleton "q5")]
      q5Map = Map.fromList [('0', Set.singleton "q5"), ('1', Set.singleton "q5")]
      tm = Map.fromList [(Set.singleton "q0", q0Map), (Set.fromList ["q1", "q2"], q12Map), (Set.fromList ["q3", "q4"], q34Map), (Set.singleton "q5", q5Map)]
      ss = Set.singleton "q0"
      as = Set.fromList [Set.fromList ["q1", "q2"], Set.singleton "q5"]
   in F s a tm ss as

d6' :: DFA (Set String)
d6' =
  let s = Set.fromList [Set.fromList ["a", "b"], Set.fromList ["c", "d", "e"], Set.singleton "f"]
      a = Set.fromList ['0', '1']
      abMap = Map.fromList [('0', Set.fromList ["a", "b"]), ('1', Set.fromList ["c", "d", "e"])]
      cdeMap = Map.fromList [('0', Set.fromList ["c", "d", "e"]), ('1', Set.singleton "f")]
      fMap = Map.fromList [('0', Set.singleton "f"), ('1', Set.singleton "f")]
      tm = Map.fromList [(Set.fromList ["a", "b"], abMap), (Set.fromList ["c", "d", "e"], cdeMap), (Set.singleton "f", fMap)]
      ss = Set.fromList ["a", "b"]
      as = Set.singleton (Set.fromList ["c", "d", "e"])
   in F s a tm ss as

d7' :: DFA (Set String)
d7' =
  let s = Set.fromList [Set.singleton "a", Set.fromList ["b", "d"], Set.singleton "c", Set.singleton "e"]
      a = Set.fromList ['a', 'b']
      aMap = Map.fromList [('a', Set.fromList ["b", "d"]), ('b', Set.fromList ["b", "d"])]
      bdMap = Map.fromList [('a', Set.singleton "c"), ('b', Set.singleton "e")]
      cMap = Map.fromList [('a', Set.fromList ["b", "d"]), ('b', Set.singleton "e")]
      eMap = Map.fromList [('a', Set.singleton "e"), ('b', Set.singleton "e")]
      tm = Map.fromList [(Set.singleton "a", aMap), (Set.fromList ["b", "d"], bdMap), (Set.singleton "c", cMap), (Set.singleton "e", eMap)]
      ss = Set.singleton "a"
      as = Set.fromList [Set.singleton "c", Set.singleton "e"]
   in F s a tm ss as

test_minimizeDFA :: Test
test_minimizeDFA =
  "minimizeDFA tests"
    ~: TestList
      [ minimizeDFA d5 ~?= d5',
        minimizeDFA d6 ~?= d6',
        minimizeDFA d7 ~?= d7'
      ]

prop_equivalent :: Ord a => NFA a -> Property
prop_equivalent nfa =
  let dfa = toDFA nfa
      minDfa = minimizeDFA dfa
   in classify (Set.size (states minDfa) < Set.size (states dfa)) "non-trivial" $ equivalentDFA dfa minDfa

prop_isDFA :: Ord a => NFA a -> Property
prop_isDFA nfa =
  let dfa = toDFA nfa
      minDfa = minimizeDFA dfa
   in classify (Set.size (states minDfa) < Set.size (states dfa)) "non-trivial" $ prop_ValidDFA dfa

prop_isSmaller :: Ord a => NFA a -> Property
prop_isSmaller nfa =
  let dfa = toDFA nfa
      minDfa = minimizeDFA dfa
   in classify (Set.size (states minDfa) < Set.size (states dfa)) "non-trivial" $ Set.size (states minDfa) <= Set.size (states dfa)

prop_isMinimal :: Ord a => NFA a -> Property
prop_isMinimal nfa =
  let dfa = toDFA nfa
      minDfa = minimizeDFA dfa
      minDfa' = minimizeDFA minDfa
   in classify (Set.size (states minDfa) < Set.size (states dfa)) "non-trivial" $ Set.size (states minDfa) == Set.size (states minDfa')