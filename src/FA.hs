module FA where

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

{-
type State = (Bool, Int)
type NFA = Map State (Map Char (Set State))
type DFA = Map State (Map Char State) -- should we create a new type for DFA's?
-}
-- type Symbol = Char
-- Should the adjacentList be a map or a function with type State -> Char -> Set State?

data FA a s = F {states :: Set a, alphabet :: Set Char, transitionMap :: Map a s, startState :: a, acceptStates :: Set a} deriving (Eq, Show)

type NFA a = FA a (Map Symbol (Set a))

type DFA a = FA a (Map Char a)

data Symbol
  = Char Char
  | Epsilon
  deriving (Eq, Ord, Show)

-- Given an nfa n, state st, and symbol s, it returns the set of neighbors that can be
-- reached after reading s in st.
findNeighborsN :: Ord a => NFA a -> a -> Symbol -> Set a
findNeighborsN nfa state symbol = (transitionMap nfa ! state) ! symbol

-- Perfoms bfs to find the states reachable by only transitioning along epsilon edges
findEpsilonStates :: Ord a => NFA a -> Set a -> [a] -> Set a
findEpsilonStates nfa visited [] = visited
findEpsilonStates nfa visited (q : qs) =
  let neighbors = findNeighborsN nfa q Epsilon
      unvisitedNeighbors = Set.filter (`Set.notMember` visited) neighbors
   in findEpsilonStates nfa (visited <> unvisitedNeighbors) (qs ++ Set.toList unvisitedNeighbors)

-- Given an nfa n and state st, it returns the set of states reachable by transitioning
-- only on epsilon edges from st.
transitionEpsilon :: Ord a => NFA a -> a -> Set a
transitionEpsilon nfa state = findEpsilonStates nfa (Set.singleton state) [state]

-- Given a function f :: a -> Set a and a set s :: (Set a), it returns the set that is formed
-- by applying f to each element in s to get s' :: (Set (Set a)), and then flattens s'
flattenMap :: Ord a => (a -> Set a) -> Set a -> Set a
flattenMap f = foldr ((<>) . f) Set.empty

-- Given an nfa n, state st, and char c, it returns the set of states that can be reached
-- from st on c.
transitionN :: Ord a => NFA a -> a -> Char -> Set a
transitionN nfa state char =
  let epsilonStates = transitionEpsilon nfa state
      charStates = flattenMap (\s -> findNeighborsN nfa s (Char char)) epsilonStates
   in flattenMap (transitionEpsilon nfa) charStates

-- Given an nfa n, state st and string s, it returns the set of states that can be reached
-- from st on s.
stringTransitionN :: forall a. Ord a => NFA a -> a -> String -> Set a
stringTransitionN nfa state [] = transitionEpsilon nfa state
stringTransitionN nfa state (x : xs) = go (transitionN nfa state x) xs
  where
    go :: Ord a => Set a -> String -> Set a
    go states [] = states
    go states (c : cs) =
      let charStates = flattenMap (\s -> transitionN nfa s c) states
       in go charStates cs

-- Returns true iff the NFA accepts the string
acceptN :: Ord a => NFA a -> String -> Bool
acceptN nfa s = Set.intersection (stringTransitionN nfa (startState nfa) s) (acceptStates nfa) /= Set.empty

-- Given a dfa d, state st, and char c, it returns the state that can be reached
-- from st on c.
transitionD :: Ord a => DFA a -> a -> Char -> a
transitionD dfa state char = (transitionMap dfa ! state) ! char

-- Given a dfa d, state st, and string s, it returns the state that can be reached
-- from st on s.
stringTransitionD :: Ord a => DFA a -> a -> String -> a
stringTransitionD dfa state [] = state
stringTransitionD dfa state (x : xs) =
  let nextState = transitionD dfa state x
   in stringTransitionD dfa nextState xs

-- Returns true iff the DFA accepts the string
acceptD :: Ord a => DFA a -> String -> Bool
acceptD dfa s =
  let endState = stringTransitionD dfa (startState dfa) s
   in Set.member endState (acceptStates dfa)

-- Performs bfs on a dfa
bfsD :: Ord a => DFA a -> Set a -> [a] -> Set a
bfsD dfa visited [] = visited
bfsD dfa visited (q : qs) =
  let neighbors = Set.foldr (Set.insert . transitionD dfa q) Set.empty (alphabet dfa)
      unvisitedNeighbors = Set.filter (`Set.notMember` visited) neighbors
   in bfsD dfa (visited <> unvisitedNeighbors) (qs ++ Set.toList unvisitedNeighbors)

-- Performs bfs on dfa d from the start state of d to find the reachable states
findReachableStatesD :: Ord a => DFA a -> Set a
findReachableStatesD dfa = bfsD dfa (Set.singleton (startState dfa)) [startState dfa]

-- Removes the unreachable states from a dfa
removeUnreachableStatesD :: Ord a => DFA a -> DFA a
removeUnreachableStatesD dfa =
  let rs = findReachableStatesD dfa
      s = Set.intersection rs (states dfa)
      a = alphabet dfa
      tm = Map.filterWithKey (\k _ -> Set.member k rs) (transitionMap dfa)
      ss = startState dfa
      as = Set.intersection (acceptStates dfa) rs
   in F s a tm ss as

-- Performs bfs on an nfa
bfsN :: Ord a => NFA a -> Set a -> [a] -> Set a
bfsN nfa visited [] = visited
bfsN nfa visited (q : qs) =
  let symbols = Set.insert Epsilon (Set.map Char (alphabet nfa))
      neighbors = Set.foldr (\x y -> findNeighborsN nfa q x <> y) Set.empty symbols
      unvisitedNeighbors = Set.filter (`Set.notMember` visited) neighbors
   in bfsN nfa (visited <> unvisitedNeighbors) (qs ++ Set.toList unvisitedNeighbors)

-- Performs bfs on an nfa n from the start state of n to find the reachable states
findReachableStatesN :: Ord a => NFA a -> Set a
findReachableStatesN nfa = bfsN nfa (Set.singleton (startState nfa)) [startState nfa]

-- Removes the unreachable states from an nfa
removeUnreachableStatesN :: Ord a => NFA a -> NFA a
removeUnreachableStatesN nfa =
  let rs = findReachableStatesN nfa
      s = Set.intersection rs (states nfa)
      a = alphabet nfa
      tm = Map.filterWithKey (\k _ -> Set.member k rs) (transitionMap nfa)
      ss = startState nfa
      as = Set.intersection (acceptStates nfa) rs
   in F s a tm ss as

-- -- is this even a function we can write? (potentially infinite language)
-- -- this would need to be a potentially infinite set / infinite list
-- language :: NFA -> List String
-- language = undefined

-- -- Is the language of this NFA the empty set?
-- isVoid :: NFA -> Bool
-- isVoid = undefined

{-
isVoid n = lanauge n == empty set
alternatively, do the algorithm from Sipser
    do any reachability algorithm
    if at any point the set of reachable states contains an accepting state return true
-}
