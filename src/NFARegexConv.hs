module NFARegexConv where

import FA
import NFAOperations
import RegExp

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

toNFA :: RegExp -> NFA Int

-- Formally, N = ({q1,q2}, Σ, δ, q1, {q2}), where we describe δ by saying
-- that δ(q1,a) = {q2}and that δ(r,b) = ∅for r 6= q1 or b 6= a
-- TODO either change the type of RegExp char to not be a set or make a smart constructor
-- to guarantee that this is a singleton
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

toNFA (Alt r1 r2) = toNFA r1 `union` toNFA r2

toNFA (Append r1 r2) = toNFA r1 `concatenate` toNFA r2

toNFA (Star r) = NFAOperations.star (toNFA r) 


-- todo change the type of regexp char to just take a single char
-- toGNFA :: DFA a -> (NFA a or GNFA a)
toRegExp :: NFA a -> RegExp
toRegExp = undefined