module FA where

import qualified Control.Applicative as S
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
  deriving (Eq, Ord)

findNeighborsN :: Ord a => NFA a -> a -> Symbol -> Set a
findNeighborsN nfa state symbol = (transitionMap nfa ! state) ! symbol

findEpsilonStates :: Ord a => NFA a -> Set a -> [a] -> Set a
findEpsilonStates nfa visited queue@[] = visited
findEpsilonStates nfa visited queue@(q : qs) =
  let neighbors = findNeighborsN nfa q Epsilon
   in case Set.toList neighbors of
        [] -> findEpsilonStates nfa visited qs
        n : ns ->
          if Set.member n visited
            then findEpsilonStates nfa visited qs
            else findEpsilonStates nfa (Set.insert n visited) (qs ++ [n])

transitionEpsilon :: Ord a => NFA a -> a -> Set a
transitionEpsilon nfa state = findEpsilonStates nfa (Set.singleton state) [state]

flattenMap :: Ord a => (a -> Set a) -> Set a -> Set a
flattenMap f = foldr ((<>) . f) Set.empty

transitionN :: Ord a => NFA a -> a -> Char -> Set a
transitionN nfa state char =
  let epsilonStates = transitionEpsilon nfa state
      charStates = flattenMap (\s -> findNeighborsN nfa s (Char char)) epsilonStates
   in flattenMap (transitionEpsilon nfa) charStates

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
acceptN nfa xs = Set.intersection (stringTransitionN nfa (startState nfa) xs) (acceptStates nfa) /= Set.empty

transitionD :: Ord a => DFA a -> a -> Char -> Maybe a
transitionD dfa state char = do
  neighbors <- transitionMap dfa !? state
  neighbors !? char

stringTransitionD :: Ord a => DFA a -> a -> String -> Maybe a
stringTransitionD dfa state [] = return state
stringTransitionD dfa state (x : xs) = do
  nextState <- transitionD dfa state x
  stringTransitionD dfa nextState xs

-- Returns true iff the DFA accepts the string
acceptD :: Ord a => DFA a -> String -> Maybe Bool
acceptD dfa xs = do
  endState <- stringTransitionD dfa (startState dfa) xs
  return $ Set.member endState (acceptStates dfa)

isDFA :: DFA a -> Bool
isDFA = undefined

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
