module NFAOperations where

import FA
import RegExp

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

shiftStatesNFA :: Int -> NFA Int -> NFA Int
shiftStatesNFA k (F s a tm ss as) =
    let shift = (+ k)
        s' = Set.map shift s
        a' = a
        tm' = Map.mapKeys shift (Map.map (Map.map (Set.map shift)) tm)
        ss' = shift ss
        as' = Set.map shift as in
    F s' a' tm' ss' as'

-- test_shiftStatesNFA :: Test
-- test_shiftStatesNFA =
--   "shift states tests"
--     ~: TestList
--       [ states (shiftStatesNFA 5 n1) ~?= Set.fromList [6, 7, 8, 9],
--         transitionMap (shiftStatesNFA 5 n1) ! 6 ~?= Map.fromList [(FA.Char '0', Set.singleton 6), (FA.Char '1', Set.fromList [6, 7]), (Epsilon, Set.empty)], 
--         transitionMap (shiftStatesNFA 5 n1) ! 7 ~?= Map.fromList [(FA.Char '0', Set.singleton 8), (FA.Char '1', Set.empty), (Epsilon, Set.singleton 8)], 
--         transitionMap (shiftStatesNFA 5 n1) ! 8 ~?= Map.fromList [(FA.Char '0', Set.empty), (FA.Char '1', Set.singleton 9), (Epsilon, Set.empty)],
--         transitionMap (shiftStatesNFA 5 n1) ! 9 ~?= Map.fromList [(FA.Char '0', Set.singleton 9), (FA.Char '1', Set.singleton 9), (Epsilon, Set.empty)],
--         startState (shiftStatesNFA 5 n1) ~?= 6,
--         acceptStates (shiftStatesNFA 5 n1) ~?= Set.singleton 9
--       ]

-- Creates a transition map for an NFA that, for each state, maps each symbol to the empty set 
emptyTransitionMapNFA :: Ord a => Set a -> Set Char -> Map a (Map Symbol (Set a))
emptyTransitionMapNFA states alphabet =
    Map.fromList (zip (Set.toList states) (repeat (Map.fromList (zip (Set.toList symbols) (repeat Set.empty)))))
    where symbols = Set.insert Epsilon (Set.map FA.Char alphabet)

-- Creates a copy of the transition map that contains keys and values for each state and symbol
makeTotalTransitionMapNFA :: Ord a => Set a -> Set Char -> Map a (Map Symbol (Set a)) -> Map a (Map Symbol (Set a))
makeTotalTransitionMapNFA qs sigma delta = Map.unionWith Map.union delta (emptyTransitionMapNFA qs sigma)

-- Creates a transition map for the union of two NFAs, given the new states, alphabet, and start state
unionTransitionMaps :: Ord a => NFA a -> NFA a -> Set a -> Set Char -> a -> Map a (Map Symbol (Set a))
unionTransitionMaps n1 n2 qs sigma q0 =
  let deltaUnion = Map.union (transitionMap n1) (transitionMap n2)
      q0Map = Map.singleton Epsilon (Set.fromList [startState n1, startState n2])
      deltaWithq0 = Map.insert q0 q0Map deltaUnion
      delta = makeTotalTransitionMapNFA qs sigma deltaWithq0
   in delta

-- Creates the union of two NFAs
union :: NFA Int -> NFA Int -> NFA Int
union n1 n2 = 
    let n1' = if Set.findMin (states n1) == 0 then shiftStatesNFA 1 n1 else n1 
        n2' = shiftStatesNFA (Set.findMax (states n1') - Set.findMin (states n2) + 1) n2 
        qs = Set.unions [Set.singleton q0, states n1', states n2']
        sigma = Set.union (alphabet n1') (alphabet n2')
        delta = unionTransitionMaps n1' n2' qs sigma q0 
        q0 = 0 
        fs = Set.union (acceptStates n1') (acceptStates n2')
     in F qs sigma delta q0 fs

-- Adds epsilon transitions from *srcs* to *tgt* in the map
addEpsilonTransitions :: Ord a => Set a -> a -> Map a (Map Symbol (Set a)) -> Map a (Map Symbol (Set a))
addEpsilonTransitions srcs tgt delta = foldr (Map.adjust (Map.insertWith Set.union Epsilon (Set.singleton tgt))) delta srcs

-- Creates a transition map for the concatenation of two NFAs, given the new states and alphabet
concatenateTransitionMaps :: Ord a => NFA a -> NFA a -> Set a -> Set Char -> Map a (Map Symbol (Set a))
concatenateTransitionMaps n1 n2 qs sigma = 
  let deltaUnion = Map.union (transitionMap n1) (transitionMap n2)
      deltaWithEpsilons = addEpsilonTransitions (acceptStates n1) (startState n2) deltaUnion
      delta = makeTotalTransitionMapNFA qs sigma deltaWithEpsilons
   in delta

-- Concatenates two NFAs
concatenate :: NFA Int -> NFA Int -> NFA Int
concatenate n1 n2 = 
  let n2' = shiftStatesNFA (Set.findMax (states n1) - Set.findMin (states n2) + 1) n2 
      qs = Set.union (states n1) (states n2')
      sigma = Set.union (alphabet n1) (alphabet n2')
      delta = concatenateTransitionMaps n1 n2' qs sigma
      q0 = startState n1
      fs = acceptStates n2' 
   in F qs sigma delta q0 fs

-- Creates a transition map for the star of an NFA, given the new states, alphabet, and start state
starTransitionMap :: Ord a => NFA a -> Set a -> Set Char -> a -> Map a (Map Symbol (Set a))
starTransitionMap n qs sigma q0 = 
  let q0Map = Map.singleton Epsilon (Set.singleton (startState n))
      deltaWithq0 = Map.insert q0 q0Map (transitionMap n)
      deltaWithEpsilons = addEpsilonTransitions (acceptStates n) (startState n) deltaWithq0
      delta = makeTotalTransitionMapNFA qs sigma deltaWithEpsilons
   in delta

-- Creates the star NFA
star :: NFA Int -> NFA Int
star n =
    let n' = if Set.findMin (states n) == 0 then shiftStatesNFA 1 n else n
        qs = Set.insert q0 (states n')
        sigma = alphabet n
        delta = starTransitionMap n' qs sigma q0
        q0 = 0
        fs = Set.insert q0 (acceptStates n')
     in F qs sigma delta q0 fs
