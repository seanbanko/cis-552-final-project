module Main where

import qualified FATests
import qualified Generators
import qualified NFADFAConvTests
import qualified DFAMinimizationTests
import qualified NFARegexConvTests

main :: IO ()
main = do
  putStrLn "FA Tests"
  FATests.runTests
  putStrLn "Generators Tests"
  Generators.runTests
  putStrLn "NFA DFA Conversion Tests"
  NFADFAConvTests.runTests
  putStrLn "DFA Minimization Tests"
  DFAMinimizationTests.runTests
  putStrLn "NFA RegExp Conversion Tests"
  NFARegexConvTests.runTests