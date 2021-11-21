module DFAMinimizationTests where 

import DFAMinimization
import NFA 
import Test.HUnit
import Test.QuickCheck
import Generators
import Data.Set (Set)
import qualified Data.Set as Set

-- checks if DFA d and (minimize d) accept the same language 
prop_equivalent :: NFA -> Bool 
prop_equivalent d = undefined 

prop_isDFA :: NFA -> Bool 
prop_isDFA d = isDFA (minimizeDFA d) 

prop_isSmaller :: NFA -> Bool 
prop_isSmaller d = Set.size (states (minimizeDFA d)) <= Set.size (states d)