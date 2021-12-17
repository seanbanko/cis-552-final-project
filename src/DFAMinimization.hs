{-# LANGUAGE MultiWayIf #-}

module DFAMinimization where

import Data.List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import FA

-- Initializes table for marking algorithm. A pair (s1, s2) is marked iff
-- s1 is not an accept state and s2 is accept state.
initializeTable :: forall a. Ord a => DFA a -> Map (a, a) Bool
initializeTable dfa =
  let pairStates = Set.filter (uncurry (<)) (Set.cartesianProduct (states dfa) (states dfa))
   in Set.foldr
        ( \(s1, s2) y ->
            let s1' = Set.member s1 (acceptStates dfa)
                s2' = Set.member s2 (acceptStates dfa)
             in Map.insert (s1, s2) (s1' /= s2') y
        )
        Map.empty
        pairStates

-- Performs the markings for each iteration of the algorithm
makeMarkings :: Ord a => DFA a -> Map (a, a) Bool -> Map (a, a) Bool
makeMarkings dfa table =
  Map.foldrWithKey
    ( \k v m ->
        if not v
          then Map.insert k (canMark dfa table k) m
          else Map.insert k True m
    )
    Map.empty
    table

-- Let delta be transition function and (s1, s2) be a pair of states.
-- Determines if a pair of states can be marked by  checking that for
--  any character c, delta(s1, c) and delta(s2, c) is marked.
canMark :: Ord a => DFA a -> Map (a, a) Bool -> (a, a) -> Bool
canMark dfa table (s1, s2) =
  any
    ( \x ->
        let s1' = transitionD dfa s1 x
            s2' = transitionD dfa s2 x
         in if
                | s1' == s2' -> False
                | s1' < s2' -> table ! (s1', s2')
                | otherwise -> table ! (s2', s1')
    )
    (alphabet dfa)

-- Performs convergence on the makeMarkings function
convergeTable :: Ord a => DFA a -> Map (a, a) Bool -> Map (a, a) Bool
convergeTable dfa = until (\x -> makeMarkings dfa x == x) (makeMarkings dfa)

-- Returns a list of pairs of states that are equivalent after the table converges
findEquivStatePairs :: Ord a => Map (a, a) Bool -> [Set a]
findEquivStatePairs = Map.foldrWithKey (\(s1, s2) v equivStates -> if not v then Set.fromList [s1, s2] : equivStates else equivStates) []

-- Given a list of equivalent states, it outputs the transitive closure
transitiveClosure :: Ord a => [Set a] -> Set (Set a) -> Set (Set a)
transitiveClosure [] prevTC = prevTC
transitiveClosure (x : xs) prevTC =
  let (p1, p2) = Set.partition (not . Set.disjoint x) prevTC
      newTC = flatten p1
   in transitiveClosure xs (Set.insert newTC p2)

-- Creates a map from the original dfa's state to its new state in the minimized dfa
newStatesMap :: Ord a => DFA a -> Map a (Set a)
newStatesMap dfa =
  let finalTable = convergeTable dfa (initializeTable dfa)
      equivStatePairs = findEquivStatePairs finalTable
      tc = transitiveClosure equivStatePairs (Set.fromList equivStatePairs)
   in Set.foldr
        ( \x y ->
            let equivStates = findEquivStates x tc
             in if equivStates == Set.empty
                  then Map.insert x (Set.singleton x) y
                  else Map.insert x equivStates y
        )
        Map.empty
        (states dfa)
  where
    findEquivStates :: Ord a => a -> Set (Set a) -> Set a
    findEquivStates state equivStates = flatten $ Set.filter (Set.member state) equivStates

-- Returns the states of the mimimized dfa
changeStates :: Ord a => DFA a -> Map a (Set a) -> Set (Set a)
changeStates dfa = Map.foldr Set.insert Set.empty

-- Returns the new start state of the minimized dfa
changeStartState :: Ord a => DFA a -> Map a (Set a) -> Set a
changeStartState dfa statesMap = statesMap ! startState dfa

-- Returns the new accept states of the mimimized dfa
changeAcceptStates :: Ord a => DFA a -> Map a (Set a) -> Set (Set a)
changeAcceptStates dfa = Map.foldrWithKey (\k v y -> if Set.member k (acceptStates dfa) then Set.insert v y else y) Set.empty

-- Returns the new transition map of the mimimized dfa
changeTransitions :: Ord a => DFA a -> Map a (Set a) -> Set (Set a) -> Map (Set a) (Map Char (Set a))
changeTransitions dfa statesMap = Set.foldr (\x y -> Map.insert x (createCharMap dfa statesMap x) y) Map.empty
  where
    newStateTransition :: Ord a => DFA a -> Map a (Set a) -> Set a -> Char -> Set a
    newStateTransition dfa statesMap newState char =
      case Set.toList newState of
        [] -> Set.empty
        (x : xs) -> statesMap ! transitionD dfa x char
    createCharMap :: Ord a => DFA a -> Map a (Set a) -> Set a -> Map Char (Set a)
    createCharMap dfa statesMap newState = Set.foldr (\x y -> Map.insert x (newStateTransition dfa statesMap newState x) y) Map.empty (alphabet dfa)

-- Minimizes the dfa by first removing the unreachable states and performing the above algorithm
minimizeDFA :: forall a. Ord a => DFA a -> DFA (Set a)
minimizeDFA dfa =
  let dfa' = removeUnreachableStatesD dfa
      statesMap = newStatesMap dfa'
      s = changeStates dfa' statesMap
      tm = changeTransitions dfa' statesMap s
      ss = changeStartState dfa' statesMap
      as = changeAcceptStates dfa' statesMap
   in dfa' {states = s, transitionMap = tm, startState = ss, acceptStates = as}
