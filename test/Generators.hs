module Generators where

import Data.Map (Map)
import qualified Data.Map as Map
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

genStates :: forall a. (Ord a, Arbitrary a) => Gen (Set a)
genStates = Set.fromList <$> listOf1 (arbitrary :: Gen a)

genAlphabet :: Gen (Set Char)
genAlphabet = elements (Set.toList (Set.powerSet (Set.fromList ['a', 'b', 'c'])))

genSubset :: Set a -> Gen (Set a)
genSubset set = elements (Set.toList (Set.powerSet set))

genStartState :: Set a -> Gen a
genStartState states = do
  i <- chooseInt (0, Set.size states - 1)
  return $ Set.elemAt i states

instance (Arbitrary a, Ord a) => Arbitrary (NFA a) where
  arbitrary :: Gen (NFA a)
  arbitrary =
    do
      s <- genStates
      a <- genAlphabet
      tm <- genTransitionMapN s a
      ss <- genStartState s
      as <- genSubset s
      return $ F s a tm ss as
    where
      genSymbolMap :: a -> Set a -> Set Char -> Gen (Map Symbol (Set a))
      genSymbolMap state states alphabet =
        let symbols = Set.insert Epsilon (Set.map Char alphabet)
            gens = Set.foldr (\x y -> genSubset states : y) [] symbols
         in f symbols <$> sequence gens
        where
          f :: Set Symbol -> [Set a] -> Map Symbol (Set a)
          f symbols stateSubsets = Map.fromList (zip (Set.toList symbols) stateSubsets)

      genTransitionMapN :: Set a -> Set Char -> Gen (Map a (Map Symbol (Set a)))
      genTransitionMapN states alphabet =
        let gens = Set.foldr (\x y -> genSymbolMap x states alphabet : y) [] states
         in f states <$> sequence gens
        where
          f :: Set a -> [Map Symbol (Set a)] -> Map a (Map Symbol (Set a))
          f states maps = Map.fromList (zip (Set.toList states) maps)

instance (Arbitrary a, Ord a) => Arbitrary (DFA a) where
  arbitrary :: Gen (DFA a)
  arbitrary =
    do
      s <- genStates
      a <- genAlphabet
      tm <- genTransitionMapD s a
      ss <- genStartState s
      as <- genSubset s
      return $ F s a tm ss as
    where
      genCharMap :: a -> Set a -> Set Char -> Gen (Map Char a)
      genCharMap state states alphabet =
        let gens = Set.foldr (\x y -> (arbitrary :: Gen a) : y) [] alphabet
         in f alphabet <$> sequence gens
        where
          f :: Set Char -> [a] -> Map Char a
          f alphabet randomStates = Map.fromList (zip (Set.toList alphabet) randomStates)

      genTransitionMapD :: Set a -> Set Char -> Gen (Map a (Map Char a))
      genTransitionMapD states alphabet =
        let gens = Set.foldr (\x y -> genCharMap x states alphabet : y) [] states
         in f states <$> sequence gens
        where
          f :: Set a -> [Map Char a] -> Map a (Map Char a)
          f states maps = Map.fromList (zip (Set.toList states) maps)

{-
-- Generator for strings accepted by this NFA (if any)
genNFAString :: NFA -> Maybe (Gen String)
-}

-- Returns true iff the NFA nfa satisfies all validity properties
prop_ValidNFA :: Ord a => NFA a -> Bool
prop_ValidNFA nfa =
  let s = states nfa
      a = alphabet nfa
      tm = transitionMap nfa
      ss = startState nfa
      as = acceptStates nfa
   in Set.member ss s && Set.isSubsetOf as s && (Set.fromList (Map.keys tm) == s)
        && all (validSymbolMap s a) tm
  where
    validSymbolMap :: Ord a => Set a -> Set Char -> Map Symbol (Set a) -> Bool
    validSymbolMap states alphabet map =
      let symbols = Set.insert Epsilon (Set.map Char alphabet)
       in (Set.fromList (Map.keys map) == symbols)
            && all (`Set.isSubsetOf` states) map

-- Returns true iff the DFA dfa satisfies all validity properties
prop_ValidDFA :: Ord a => DFA a -> Bool
prop_ValidDFA dfa =
  let s = states dfa
      a = alphabet dfa
      tm = transitionMap dfa
      ss = startState dfa
      as = acceptStates dfa
   in Set.member ss s && Set.isSubsetOf as s && (Set.fromList (Map.keys tm) == s)
        && all (validCharMap s a) tm
  where
    validCharMap :: Ord a => Set a -> Set Char -> Map Char a -> Bool
    validCharMap states alphabet map =
      (Set.fromList (Map.keys map) == alphabet)
        && all (`Set.member` states) map

quickCheckN :: (Testable prop) => Int -> prop -> IO ()
quickCheckN n = quickCheck . withMaxSuccess n

genString :: FA a s -> Gen String
genString fa = listOf (elements (Set.toList (alphabet fa)))