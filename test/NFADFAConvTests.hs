module NFADFAConvTests where 

import NFA
import NFADFAConv
import Test.HUnit
import Test.QuickCheck
import Generators 

-- check if the NFA n and (toDFA n) accept the same language
prop_equivalent :: NFA -> Bool
prop_equivalent n = undefined 

prop_isDFA :: NFA -> Bool 
prop_isDFA n = isDFA (toDFA n) 

