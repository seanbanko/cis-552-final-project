module NFAOperations where

import FA
import RegExp

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

-- "Instance" of fmap for an NFA
fmapNFA :: (Ord a, Ord b) => (a -> b) -> NFA a -> NFA b
fmapNFA f (F s a tm ss as) = 
  let s'    = Set.map f s
      a'    = a
      tm'   = Map.mapKeys f (Map.map (Map.map (Set.map f)) tm)
      ss'   = f ss
      as'   = Set.map f as
    in F s' a' tm' ss' as'

-- "Instance" of fmap for a DFA
fmapDFA :: (Ord a, Ord b) => (a -> b) -> DFA a -> DFA b
fmapDFA f (F s a tm ss as) = 
  let s'    = Set.map f s
      a'    = a
      tm'   = Map.mapKeys f (Map.map (Map.map f) tm)
      ss'   = f ss
      as'   = Set.map f as
    in F s' a' tm' ss' as'

-- Renames the states of an NFA, incrementing them by k
shiftStatesNFA :: Int -> NFA Int -> NFA Int
shiftStatesNFA k = fmapNFA (+ k)

-- Creates a transition map for an NFA that, for each state, maps each symbol to the empty set 
emptyTransitionMapNFA :: Ord a => Set a -> Set Char -> Map a (Map Symbol (Set a))
emptyTransitionMapNFA states alphabet =
  let symbols = Set.insert Epsilon (Set.map FA.Char alphabet) in
    Map.fromList (zip (Set.toList states) (repeat (Map.fromList (zip (Set.toList symbols) (repeat Set.empty)))))

-- Creates a copy of the transition map that contains keys and values for each state and symbol
makeTotalTransitionMapNFA :: Ord a => Set a -> Set Char -> Map a (Map Symbol (Set a)) -> Map a (Map Symbol (Set a))
makeTotalTransitionMapNFA qs sigma delta = Map.unionWith Map.union delta (emptyTransitionMapNFA qs sigma)

-- Creates a transition map for the union of two NFAs, given the new states, alphabet, and start state
unionTransitionMaps :: Ord a => NFA a -> NFA a -> Set a -> Set Char -> a -> Map a (Map Symbol (Set a))
unionTransitionMaps n1 n2 qs sigma q0 =
  let deltaUnion  = Map.union (transitionMap n1) (transitionMap n2)
      q0Map       = Map.singleton Epsilon (Set.fromList [startState n1, startState n2])
      deltaWithq0 = Map.insert q0 q0Map deltaUnion
      delta       = makeTotalTransitionMapNFA qs sigma deltaWithq0
   in delta

-- Creates the union of two NFAs
union :: NFA Int -> NFA Int -> NFA Int
union n1 n2 = 
    let n1'   = n1 
        n2'   = shiftStatesNFA (Set.findMax (states n1') - Set.findMin (states n2) + 1) n2 
        qs    = Set.unions [Set.singleton q0, states n1', states n2']
        sigma = Set.union (alphabet n1') (alphabet n2')
        delta = unionTransitionMaps n1' n2' qs sigma q0 
        q0    = max (Set.findMax (states n1')) (Set.findMax (states n2')) + 1 
        fs    = Set.union (acceptStates n1') (acceptStates n2')
     in F qs sigma delta q0 fs

-- Adds epsilon transitions from *srcs* to *tgt* in the map
addEpsilonTransitions :: Ord a => Set a -> a -> Map a (Map Symbol (Set a)) -> Map a (Map Symbol (Set a))
addEpsilonTransitions srcs tgt delta = foldr (Map.adjust (Map.insertWith Set.union Epsilon (Set.singleton tgt))) delta srcs

-- Creates a transition map for the concatenation of two NFAs, given the new states and alphabet
concatenateTransitionMaps :: Ord a => NFA a -> NFA a -> Set a -> Set Char -> Map a (Map Symbol (Set a))
concatenateTransitionMaps n1 n2 qs sigma = 
  let deltaUnion        = Map.union (transitionMap n1) (transitionMap n2)
      deltaWithEpsilons = addEpsilonTransitions (acceptStates n1) (startState n2) deltaUnion
      delta             = makeTotalTransitionMapNFA qs sigma deltaWithEpsilons
   in delta

-- Concatenates two NFAs
concatenate :: NFA Int -> NFA Int -> NFA Int
concatenate n1 n2 = 
  let n2'   = shiftStatesNFA (Set.findMax (states n1) - Set.findMin (states n2) + 1) n2 
      qs    = Set.union (states n1) (states n2')
      sigma = Set.union (alphabet n1) (alphabet n2')
      delta = concatenateTransitionMaps n1 n2' qs sigma
      q0    = startState n1
      fs    = acceptStates n2' 
   in F qs sigma delta q0 fs

-- Creates a transition map for the star of an NFA, given the new states, alphabet, and start state
starTransitionMap :: Ord a => NFA a -> Set a -> Set Char -> a -> Map a (Map Symbol (Set a))
starTransitionMap n qs sigma q0 = 
  let q0Map             = Map.singleton Epsilon (Set.singleton (startState n))
      deltaWithq0       = Map.insert q0 q0Map (transitionMap n)
      deltaWithEpsilons = addEpsilonTransitions (acceptStates n) (startState n) deltaWithq0
      delta             = makeTotalTransitionMapNFA qs sigma deltaWithEpsilons
   in delta

-- Creates the star NFA
star :: NFA Int -> NFA Int
star n =
    let qs    = Set.insert q0 (states n)
        sigma = alphabet n
        delta = starTransitionMap n qs sigma q0
        q0    = Set.findMax (states n) + 1
        fs    = Set.insert q0 (acceptStates n)
     in F qs sigma delta q0 fs
