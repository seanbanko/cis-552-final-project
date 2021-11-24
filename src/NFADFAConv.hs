module NFADFAConv where

import Data.Set (Set)
import qualified Data.Set as Set
import FA

-- we will incremently convert the NFA to a DFA. The alphabet will never be changed.

changeStates :: Set a -> Set (a, a)
changeStates = undefined

changeTransitions :: NFA a -> NFA a
changeTransitions = undefined

changeStartState :: NFA a -> NFA a
changeStartState = undefined

changeAcceptStates :: NFA a -> NFA a
changeAcceptStates = undefined

toDFA :: NFA a -> DFA a
toDFA nfa = undefined

-- there will be many helper functions