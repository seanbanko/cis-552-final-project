module NFA where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map, (!?))
import qualified Data.Map as Map

{-
type State = (Bool, Int) 
type NFA = Map State (Map Char (Set State))
type DFA = Map State (Map Char State) -- should we create a new type for DFA's?
-}

type State = Int
-- type Symbol = Char
data NFA = N {alphabet :: Set Char, states :: Set State, adjacencyList :: Map State (Map Char (Set State)) , start :: State, acceptStates :: Set State} deriving (Eq, Show)
-- data DFA = D {alphabet :: Set Char, states :: Set State, adjacencyList :: Map State (Map Char (Set State)) , start :: State, accept :: Set State}


-- Returns the set of states reachable from state s upon reading symbol sigma in NFA n
transition :: NFA -> State -> Char -> Maybe (Set State)
transition nfa state char = undefined
-- transition nfa state sigma = do
--    neighbors <- Map.lookup state (adjacencyList nfa)
--    return $ Map.lookup char neighbors

-- Returns true iff the NFA accepts the string
accept :: NFA -> String -> Bool
accept = undefined

isDFA :: NFA -> Bool
isDFA = undefined

-- is this even a function we can write? (potentially infinite language)
-- this would need to be a potentially infinite set / infinite list
language :: NFA -> Set String
language = undefined

-- Is the language of this NFA the empty set?
isVoid :: NFA -> Bool
isVoid = undefined

{-
isVoid n = lanauge n == empty set
alternatively, do the algorithm from Sipser
    do any reachability algorithm
    if at any point the set of reachable states contains an accepting state return true
-}