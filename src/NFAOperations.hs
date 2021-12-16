module NFAOperations where

import FA
import RegExp

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

n1 :: NFA Int
n1 =
  let s = Set.fromList [1, 2, 3, 4]
      a = Set.fromList ['0', '1']
      state1Map = Map.fromList [(FA.Char '0', Set.singleton 1), (FA.Char '1', Set.fromList [1, 2]), (Epsilon, Set.empty)]
      state2Map = Map.fromList [(FA.Char '0', Set.singleton 3), (FA.Char '1', Set.empty), (Epsilon, Set.singleton 3)]
      state3Map = Map.fromList [(FA.Char '0', Set.empty), (FA.Char '1', Set.singleton 4), (Epsilon, Set.empty)]
      state4Map = Map.fromList [(FA.Char '0', Set.singleton 4), (FA.Char '1', Set.singleton 4), (Epsilon, Set.empty)]
      tm = Map.fromList [(1, state1Map), (2, state2Map), (3, state3Map), (4, state4Map)]
      ss = 1
      as = Set.singleton 4
   in F s a tm ss as

-- nfa that accepts strings containing a 1 in the third position from the end (Sipser - page 51)
n2 :: NFA Int
n2 =
  let s = Set.fromList [1, 2, 3, 4]
      a = Set.fromList ['0', '1']
      state1Map = Map.fromList [(FA.Char '0', Set.singleton 1), (FA.Char '1', Set.fromList [1, 2]), (Epsilon, Set.empty)]
      state2Map = Map.fromList [(FA.Char '0', Set.singleton 3), (FA.Char '1', Set.singleton 3), (Epsilon, Set.empty)]
      state3Map = Map.fromList [(FA.Char '0', Set.singleton 4), (FA.Char '1', Set.singleton 4), (Epsilon, Set.empty)]
      state4Map = Map.fromList [(FA.Char '0', Set.empty), (FA.Char '1', Set.empty), (Epsilon, Set.empty)]
      tm = Map.fromList [(1, state1Map), (2, state2Map), (3, state3Map), (4, state4Map)]
      ss = 1
      as = Set.singleton 4
   in F s a tm ss as

-- instance Functor (FA a) where
--     fmap :: (a -> b) -> FA a s -> FA b s
--     fmap f (F s a tm ss as) =
--         let s' = fmap f s
--             a' = fmap f a
--             tm' = fmap f tm
--             ss' = fmap f ss
--             as' = fmap f as in
--         F s' a' tm' ss' as'


shiftStatesNFA :: Int -> NFA Int -> NFA Int
shiftStatesNFA k (F s a tm ss as) =
    let shift = (+ k)
        s' = Set.map shift s
        a' = a
        tm' = Map.mapKeys shift (Map.map (Map.map (Set.map shift)) tm)
        ss' = shift ss
        as' = Set.map shift as in
    F s' a' tm' ss' as'

test_shiftStatesNFA :: Test
test_shiftStatesNFA =
  "shift states tests"
    ~: TestList
      [ states (shiftStatesNFA 5 n1) ~?= Set.fromList [6, 7, 8, 9],
        transitionMap (shiftStatesNFA 5 n1) ! 6 ~?= Map.fromList [(FA.Char '0', Set.singleton 6), (FA.Char '1', Set.fromList [6, 7]), (Epsilon, Set.empty)], 
        transitionMap (shiftStatesNFA 5 n1) ! 7 ~?= Map.fromList [(FA.Char '0', Set.singleton 8), (FA.Char '1', Set.empty), (Epsilon, Set.singleton 8)], 
        transitionMap (shiftStatesNFA 5 n1) ! 8 ~?= Map.fromList [(FA.Char '0', Set.empty), (FA.Char '1', Set.singleton 9), (Epsilon, Set.empty)],
        transitionMap (shiftStatesNFA 5 n1) ! 9 ~?= Map.fromList [(FA.Char '0', Set.singleton 9), (FA.Char '1', Set.singleton 9), (Epsilon, Set.empty)],
        startState (shiftStatesNFA 5 n1) ~?= 6,
        acceptStates (shiftStatesNFA 5 n1) ~?= Set.singleton 9
      ]

-- Creates a transition map for an NFA that, for each state, maps each symbol to the empty set 
emptyTransitionMapNFA :: Ord a => Set a -> Set Char -> Map a (Map Symbol (Set a))
emptyTransitionMapNFA states alphabet =
    Map.fromList (zip (Set.toList states) (repeat (Map.fromList (zip (Set.toList symbols) (repeat Set.empty)))))
    where symbols = Set.insert Epsilon (Set.map FA.Char alphabet)

-- Creates a copy of the transition map that contains keys and values for each state and symbol
makeTotalTransitionMapNFA :: Ord a => Set a -> Set Char -> Map a (Map Symbol (Set a)) -> Map a (Map Symbol (Set a))
makeTotalTransitionMapNFA qs sigma delta = Map.unionWith Map.union delta (emptyTransitionMapNFA qs sigma)

-- Unions the transition maps of two NFAs, given the new states and start state
unionTransitionMaps :: Ord a => NFA a -> NFA a -> Set a -> Set Char -> a -> Map a (Map Symbol (Set a))
unionTransitionMaps n1 n2 qs sigma q0 =
  let deltaUnion = Map.union (transitionMap n1) (transitionMap n2)
      deltaWithStart = Map.insert q0 (Map.singleton Epsilon (Set.fromList [startState n1, startState n2])) deltaUnion
      delta = makeTotalTransitionMapNFA qs sigma deltaWithStart
   in delta

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

concatenate :: NFA Int -> NFA Int -> NFA Int
concatenate n1@(F s1 a1 tm1 ss1 as1) n2 = 
  let F s2 a2 tm2 ss2 as2 = shiftStatesNFA (Set.findMax s1 - Set.findMin (states n2) + 1) n2 
      sts = Set.union s1 s2
      a = Set.union a1 a2 -- TODO the union operation may not be necessary here. need/want to enforce same alphabet anyway
      ss = startState n1
      as = as2 
      tm = foldr (Map.adjust (Map.insertWith Set.union Epsilon (Set.singleton ss2))) (Map.union tm1 tm2) (acceptStates n1)
      tm' = Map.unionWith Map.union tm (emptyTransitionMapNFA sts a)
    in F sts a tm' ss as

star :: NFA Int -> NFA Int
star n =
    let F s a tm ss as = if Set.findMin (states n) == 0 then shiftStatesNFA 1 n else n -- shift the states to accomodate new start state if necessary
        s' = Set.insert ss' s
        a' = a
        -- Adds an epsilon transition from each accepting state of n to the new start state
        -- and from the new start state to the original start state
        tm' = foldr (Map.adjust (Map.insertWith Set.union Epsilon (Set.singleton ss'))) (Map.insert ss' (Map.singleton Epsilon (Set.singleton ss)) tm) as
        tm'' = Map.unionWith Map.union tm' (emptyTransitionMapNFA s' a')
        ss' = 0
        as' = Set.insert ss' as
     in F s' a' tm'' ss' as'

test_union :: Test
test_union =
  let u = union n1 n2 in
  "shift states tests"
    ~: TestList
      [ states u ~?= Set.fromList [0, 1, 2, 3, 4, 5, 6, 7, 8],
        alphabet u ~?= Set.fromList ['0', '1'],
        -- transitionMap u ! 0 ~?= Map.fromList [(Epsilon, Set.fromList [startState n1, startState n2])],
        transitionMap u ! 1 ~?= Map.fromList [(FA.Char '0', Set.singleton 1), (FA.Char '1', Set.fromList [1, 2]), (Epsilon, Set.empty)], 
        transitionMap u ! 2 ~?= Map.fromList [(FA.Char '0', Set.singleton 3), (FA.Char '1', Set.empty), (Epsilon, Set.singleton 3)], 
        transitionMap u ! 3 ~?= Map.fromList [(FA.Char '0', Set.empty), (FA.Char '1', Set.singleton 4), (Epsilon, Set.empty)], 
        transitionMap u ! 4 ~?= Map.fromList [(FA.Char '0', Set.singleton 4), (FA.Char '1', Set.singleton 4), (Epsilon, Set.empty)], 
        transitionMap u ! 5 ~?= Map.fromList [(FA.Char '0', Set.singleton 5), (FA.Char '1', Set.fromList [5, 6]), (Epsilon, Set.empty)], 
        transitionMap u ! 6 ~?= Map.fromList [(FA.Char '0', Set.singleton 7), (FA.Char '1', Set.singleton 7), (Epsilon, Set.empty)], 
        transitionMap u ! 7 ~?= Map.fromList [(FA.Char '0', Set.singleton 8), (FA.Char '1', Set.singleton 8), (Epsilon, Set.empty)], 
        transitionMap u ! 8 ~?= Map.fromList [(FA.Char '0', Set.empty), (FA.Char '1', Set.empty), (Epsilon, Set.empty)], 
        startState u ~?= 0,
        acceptStates u ~?= Set.fromList [4, 8]
      ]

-- property: any string accepted by nfa1 or nfa2 should be accepted by the union nfa
prop_union :: NFA Int -> NFA Int -> Bool
prop_union = undefined