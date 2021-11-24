module Generators where

import Data.Set (Set)
import qualified Data.Set as Set
import FA
import NFADFAConv
import Test.QuickCheck

{-
Thoughts on how to generate an arbitrary NFA:
- first generate a set of states
- from that set of states, generate a random set of transitions between the states
-}

instance Arbitrary (NFA a) where
  arbitrary :: Gen (NFA a)
  arbitrary = undefined

instance Arbitrary (DFA a) where
  arbitrary :: Gen (DFA a)
  arbitrary = toDFA <$> (arbitrary :: Gen (NFA a))

genNFAString :: NFA a -> Gen String
genNFAString nfa = listOf (elements (Set.toList (alphabet nfa)))

{-
-- Generator for strings accepted by this NFA (if any)
genNFAString :: NFA -> Maybe (Gen String)
-}

-- Returns true iff the NFA nfa satisfies all validity properties
prop_ValidNFA :: NFA a -> Bool
prop_ValidNFA nfa = undefined

-- Returns true iff the DFA dfa satisfies all validity properties
prop_ValidDFA :: DFA a -> Bool
prop_ValidDFA dfa = undefined