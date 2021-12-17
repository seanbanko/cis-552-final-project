module NFADFAConvTests where

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import FA
import FATests
import Generators
import NFADFAConv
import Test.HUnit
import Test.QuickCheck

d1 :: DFA (Set Int)
d1 = toDFA n1

d2 :: DFA (Set Int)
d2 = toDFA n2

d3 :: DFA (Set Int)
d3 = toDFA n3

d4 :: DFA (Set Int)
d4 = toDFA n4

test_toDFA :: Test
test_toDFA =
  "toDFA tests"
    ~: TestList
      [ acceptN n1 "" ~?= acceptD d1 "",
        acceptN n1 "1" ~?= acceptD d1 "1",
        acceptN n1 "00" ~?= acceptD d1 "00",
        acceptN n1 "101" ~?= acceptD d1 "101",
        acceptN n1 "11" ~?= acceptD d1 "11",
        acceptN n1 "01110" ~?= acceptD d1 "01110",
        acceptN n1 "101110" ~?= acceptD d1 "101110",
        acceptN n1 "10001" ~?= acceptD d1 "10001",
        acceptN n1 "001001" ~?= acceptD d1 "001001",
        acceptN n2 "" ~?= acceptD d2 "",
        acceptN n2 "1" ~?= acceptD d2 "1",
        acceptN n2 "01" ~?= acceptD d2 "01",
        acceptN n2 "101" ~?= acceptD d2 "101",
        acceptN n2 "0110101100" ~?= acceptD d2 "0110101100",
        acceptN n2 "0101010101001" ~?= acceptD d2 "0101010101001",
        acceptN n2 "101010101" ~?= acceptD d2 "101010101",
        acceptN n3 "" ~?= acceptD d3 "",
        acceptN n3 "0" ~?= acceptD d3 "0",
        acceptN n3 "00" ~?= acceptD d3 "00",
        acceptN n3 "000" ~?= acceptD d3 "000",
        acceptN n3 "0000" ~?= acceptD d3 "0000",
        acceptN n3 "00000" ~?= acceptD d3 "00000",
        acceptN n3 "000000" ~?= acceptD d3 "000000",
        acceptN n3 "0000000" ~?= acceptD d3 "0000000",
        acceptN n3 "00000000" ~?= acceptD d3 "00000000",
        acceptN n3 "000000000" ~?= acceptD d3 "000000000",
        acceptN n3 "0000000000" ~?= acceptD d3 "0000000000"
      ]

-- check if the NFA n and (toDFA n) accept the same language
prop_equivalent :: Ord a => NFA a -> Property
prop_equivalent nfa = forAll (genString nfa) $
  \s -> classify (acceptN nfa s) "accepting" $ acceptN nfa s == acceptD (toDFA nfa) s

prop_isDFA :: Ord a => NFA a -> Bool
prop_isDFA n = prop_ValidDFA (toDFA n)

runTests :: IO ()
runTests = do
  _ <-
    runTestTT $
      TestList
        [test_toDFA]
  quickCheck (prop_equivalent :: NFA Int -> Property)
  quickCheck (prop_isDFA :: NFA Int -> Bool)
