module Generators where 

import NFA
import Test.QuickCheck
import Data.Set (Set)
import qualified Data.Set as Set

{-
Thoughts on how to generate an arbitrary NFA:
- first generate a set of states
- from that set of states, generate a random set of transitions between the states
-}

instance Arbitrary NFA where
    -- arbitrary :: Gen NFA
    arbitrary = undefined 

-- instance arbitrary DFA where
--     arbitrary :: Gen DFA
--     arbitrary = toDFA <$> (arbitrary :: Gen NFA)


genNFAString :: NFA -> Gen String 
genNFAString nfa = listOf (elements (Set.toList (alphabet nfa))) 

{-
-- Generator for strings accepted by this NFA (if any)
genNFAString :: NFA -> Maybe (Gen String)
-}

-- Returns true iff the NFA nfa satisfies all validity properties
prop_ValidNFA :: NFA -> Bool  
prop_ValidNFA nfa = undefined

-- Returns true iff the DFA dfa satisfies all validity properties
prop_ValidDFA :: NFA -> Bool 
prop_ValidDFA dfa = undefined