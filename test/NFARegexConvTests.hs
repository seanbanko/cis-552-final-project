module NFARegexConvTests where

import FA
import Generators
import NFARegexConv
import RegExp
import Test.HUnit
import Test.QuickCheck

prop_roundTripR :: RegExp -> Bool
prop_roundTripR r = toRegExp (toNFA r) == r

-- not necessarilly equal, generate the same language

prop_roundTripN :: Eq a => NFA a -> Bool
prop_roundTripN n = toNFA (toRegExp n) == n

-- not necessarilly equal, accept the same language

-- if you convert a regexp to a nfa, then a string generated by the regexp should be accepted by the nfa
prop_acceptRegExp :: RegExp -> Bool
prop_acceptRegExp = undefined
