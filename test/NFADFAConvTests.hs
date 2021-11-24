module NFADFAConvTests where

import FA
import Generators
import NFADFAConv
import Test.HUnit
import Test.QuickCheck

-- check if the NFA n and (toDFA n) accept the same language
prop_equivalent :: NFA a -> Bool
prop_equivalent n = undefined

prop_isDFA :: NFA a -> Bool
prop_isDFA n = isDFA (toDFA n)
