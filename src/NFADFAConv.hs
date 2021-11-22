module NFADFAConv where

import NFA 

-- we will incremently convert the NFA to a DFA. The alphabet will never be changed.

changeStates :: NFA -> NFA 
changeStates = undefined 

changeTransitions :: NFA -> NFA 
changeTransitions = undefined 

changeStartState :: NFA -> NFA 
changeStartState = undefined 

changeAcceptStates :: NFA -> NFA 
changeAcceptStates = undefined 

toDFA ::  NFA -> NFA
toDFA nfa = changeAcceptStates (changeStartState (changeTransitions (changeStates nfa)))

-- there will be many helper functions