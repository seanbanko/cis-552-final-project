module DFAMinimizationTests where

import DFAMinimization
import Data.Set (Set)
import qualified Data.Set as Set
import FA
import Generators
import Test.HUnit
import Test.QuickCheck

-- checks if DFA d and (minimize d) accept the same language. we could generate
-- arbitary strings from the alphabet of d and check if accept d s == accept (minimize d) s
prop_equivalent :: Ord a => DFA a -> Property
prop_equivalent dfa = forAll (genString dfa) $
  \s -> acceptD dfa s == acceptD (minimizeDFA dfa) s

-- prop_isDFA :: DFA a -> Bool
prop_isDFA :: Ord a => DFA a -> Bool
prop_isDFA d = prop_ValidDFA (minimizeDFA d)

prop_isSmaller :: DFA a -> Bool
prop_isSmaller d = Set.size (states (minimizeDFA d)) <= Set.size (states d)