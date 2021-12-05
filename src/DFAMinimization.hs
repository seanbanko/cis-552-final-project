{-# LANGUAGE MultiWayIf #-}

module DFAMinimization where

import Data.List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import FA

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

convergeTable :: Ord a => DFA a -> Map (a, a) Bool -> Map (a, a) Bool
convergeTable dfa = until (\x -> makeMarkings dfa x == x) (makeMarkings dfa)

transitiveClosure :: Ord a => [Set a] -> Set (Set a) -> Set (Set a)
transitiveClosure [] prevTC = prevTC
transitiveClosure (x : xs) prevTC =
  let (p1, p2) = Set.partition (not . Set.disjoint x) prevTC
      newTC = flatten p1
   in transitiveClosure xs (Set.insert newTC p2)

newStatesMap :: Ord a => DFA a -> Map a (Set a)
newStatesMap dfa =
  let finalTable = convergeTable dfa (initializeTable dfa)
      equivStatePairs = Map.foldrWithKey (\(s1, s2) v setList -> if not v then Set.fromList [s1, s2] : setList else setList) [] finalTable
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

minimizeDFA :: forall a. Ord a => DFA a -> DFA (Set a)
minimizeDFA dfa =
  let statesMap = newStatesMap dfa
      s = Map.foldr Set.insert Set.empty statesMap
      tm = Set.foldr (\x y -> Map.insert x (createCharMap dfa statesMap x) y) Map.empty s
      ss = statesMap ! startState dfa
      as = Map.foldrWithKey (\k v y -> if Set.member k (acceptStates dfa) then Set.insert v y else y) Set.empty statesMap
   in F s (alphabet dfa) tm ss as
  where
    newStateTransition :: Ord a => DFA a -> Map a (Set a) -> Set a -> Char -> Set a
    newStateTransition dfa statesMap newState char = Set.foldr (\x y -> (statesMap ! transitionD dfa x char) <> y) Set.empty newState
    createCharMap :: DFA a -> Map a (Set a) -> Set a -> Map Char (Set a)
    createCharMap dfa statesMap newState = Set.foldr (\x y -> Map.insert x (newStateTransition dfa statesMap newState x) y) Map.empty (alphabet dfa)