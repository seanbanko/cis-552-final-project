module NFADFAConv where

import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import FA

-- we will incremently convert the NFA to a DFA. The alphabet will never be changed.

changeStates :: NFA a -> Set (Set a)
changeStates nfa = Set.powerSet (states nfa)

changeTransitions :: Ord a => NFA a -> Map (Set a) (Map Char (Set a))
changeTransitions nfa =
  let newStates = changeStates nfa
   in Set.foldr (\x y -> Map.insert x (createCharMap nfa x (alphabet nfa)) y) Map.empty newStates
  where
    transitionN' :: Ord a => NFA a -> a -> Char -> Set a
    transitionN' nfa state char =
      let charStates = findNeighborsN nfa state (Char char)
       in flattenMap (transitionEpsilon nfa) charStates
    newStateTransition :: Ord a => NFA a -> Set a -> Char -> Set a
    newStateTransition nfa newState char = flattenMap (\s -> transitionN' nfa s char) newState
    createCharMap :: Ord a => NFA a -> Set a -> Set Char -> Map Char (Set a)
    createCharMap nfa newState = Set.foldr (\x y -> Map.insert x (newStateTransition nfa newState x) y) Map.empty

changeStartState :: Ord a => NFA a -> Set a
changeStartState nfa = transitionEpsilon nfa (startState nfa)

changeAcceptStates :: Ord a => NFA a -> Set (Set a)
changeAcceptStates nfa =
  let newStates = changeStates nfa
   in Set.filter (\x -> Set.intersection x (acceptStates nfa) /= Set.empty) newStates

toDFA :: Ord a => NFA a -> DFA (Set a)
toDFA nfa =
  let newStates = changeStates nfa
      newTransitonMap = changeTransitions nfa
      newStartState = changeStartState nfa
      newAcceptStates = changeAcceptStates nfa
      dfa = F newStates (alphabet nfa) newTransitonMap newStartState newAcceptStates
   in removeUnreachableStatesD dfa

-- there will be many helper functions