module NFARegexConv where

import FA
import RegExp

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

-- todo change the type of regexp char to just take a single char
-- toGNFA :: DFA a -> (NFA a or GNFA a)

toRegExp :: NFA a -> RegExp
toRegExp = undefined

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

-- instance Functor (FA a) where
--     fmap :: (a -> b) -> FA a s -> FA b s
--     fmap f (F s a tm ss as) =
--         let s' = fmap f s
--             a' = fmap f a
--             tm' = fmap f tm
--             ss' = fmap f ss
--             as' = fmap f as in
--         F s' a' tm' ss' as'

shiftStates :: Int -> NFA Int -> NFA Int
shiftStates k nfa@(F s a tm ss as) =
    let shift = (+ k)
        s' = Set.map shift s
        a' = a
        tm' = Map.mapKeys shift (Map.map (Map.map (Set.map shift)) tm)
        ss' = shift ss
        as' = Set.map shift as in
    F s' a' tm' ss' as'

test_shiftStates :: Test
test_shiftStates =
  "shift states tests"
    ~: TestList
      [ states (shiftStates 5 n1) ~?= Set.fromList [6, 7, 8, 9],
        transitionMap (shiftStates 5 n1) ! 6 ~?= Map.fromList [(FA.Char '0', Set.singleton 6), (FA.Char '1', Set.fromList [6, 7]), (Epsilon, Set.empty)], 
        transitionMap (shiftStates 5 n1) ! 7 ~?= Map.fromList [(FA.Char '0', Set.singleton 8), (FA.Char '1', Set.empty), (Epsilon, Set.singleton 8)], 
        transitionMap (shiftStates 5 n1) ! 8 ~?= Map.fromList [(FA.Char '0', Set.empty), (FA.Char '1', Set.singleton 9), (Epsilon, Set.empty)],
        transitionMap (shiftStates 5 n1) ! 9 ~?= Map.fromList [(FA.Char '0', Set.singleton 9), (FA.Char '1', Set.singleton 9), (Epsilon, Set.empty)],
        startState (shiftStates 5 n1) ~?= 6,
        acceptStates (shiftStates 5 n1) ~?= Set.singleton 9
      ]

-- See Sipser page 60 Theorem 1.45
union :: NFA Int -> NFA Int -> NFA Int
union nfa1@(F s1 a1 tm1 ss1 as1) nfa2@(F s2 a2 tm2 ss2 as2) = 
    let sts = Set.unions [Set.singleton ss, s1, s2]
        a = Set.union a1 a2 -- TODO the union operation may not be necessary here. need/want to enforce same alphabet anyway
        tm = Map.union tm1 tm2 
             -- plus Map.insert ss (Map.singleton Epsilon (Set.fromList [ss1, ss2])
             -- plus Map.insert ss (Map.fromList (map (, Empty) a)) 
        ss = 0 -- TODO how to guarantee uniqueness here?
        as = Set.union as1 as2
     in F sts a tm ss as


toNFA :: RegExp -> NFA Int

-- Formally, N = ({q1,q2}, Σ, δ, q1, {q2}), where we describe δ by saying
-- that δ(q1,a) = {q2}and that δ(r,b) = ∅for r 6= q1 or b 6= a
-- TODO not sure what to do about the RegExp.char being a set not a single character
toNFA (RegExp.Char cset) = 
    let sts = Set.singleton 1
        a = cset
        tm = Map.fromList [(1, Map.fromList [(FA.Char (Set.elemAt 0 cset), Set.singleton 2)])]
        ss = 1
        as = Set.singleton 2
     in F sts a tm ss as

-- Formally, N = ({q1},Σ,δ,q1,{q1}), where δ(r,b) = ∅for any r and b.
-- TODO not sure how to properly handle mapping the empty transitions
toNFA Empty = 
    let sts = Set.singleton 1
        a = Set.empty 
        tm = Map.fromList [(1, Map.empty)]
        ss = 1
        as = Set.singleton ss
     in F sts a tm ss as

-- Formally, N = ({q},Σ,δ,q,∅), where δ(r,b) = ∅for any r and b.
-- TODO not sure how to properly handle mapping the empty transitions
toNFA Void = 
    let sts = Set.singleton 1
        a = Set.empty 
        tm = Map.fromList [(1, Map.empty)]
        ss = 1
        as = Set.empty
     in F sts a tm ss as

toNFA (Alt r1 r2) = undefined

toNFA (Append r1 r2) = undefined 

toNFA (Star r) = undefined 
