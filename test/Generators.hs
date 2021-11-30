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

genSubset :: Set a -> Gen (Set a)
genSubset set = elements (Set.toList (Set.powerSet set))

genStartState :: Set a -> Gen a
genStartState states = do
  i <- chooseInt (0, Set.size states - 1)
  return $ Set.elemAt i states

genSymbolMap :: a -> Set a -> Set Char -> Gen (Map Symbol (Set a))
genSymbolMap state states alphabet =
  let symbols = Set.insert Epsilon (Set.map Char alphabet)
      gens = Set.foldr (\x y -> genSubset states : y) [] symbols
   in f symbols <$> sequence gens
  where
    f :: Set Symbol -> [Set a] -> Map Symbol (Set a)
    f symbols stateSubset = Map.fromList (zip (Set.toList symbols) stateSubset)

genTransitonMap :: Ord a => Set a -> Set Char -> Gen (Map a (Map Symbol (Set a)))
genTransitonMap states alphabet =
  let gens = Set.foldr (\x y -> genSymbolMap x states alphabet : y) [] states
   in f states <$> sequence gens
  where
    f :: Ord a => Set a -> [Map Symbol (Set a)] -> Map a (Map Symbol (Set a))
    f states maps = Map.fromList (zip (Set.toList states) maps)

instance (Arbitrary a, Ord a) => Arbitrary (NFA a) where
  arbitrary :: Gen (NFA a)
  arbitrary =
    let genStates = Set.fromList <$> listOf1 (arbitrary :: Gen a)
        genAlphabet = Set.fromList <$> listOf1 (arbitrary :: Gen Char)
     in do
          s <- genStates
          a <- genAlphabet
          tm <- genTransitonMap s a
          ss <- genStartState s
          as <- genSubset s
          return (F s a tm ss as)

instance (Arbitrary a, Ord a) => Arbitrary (DFA (Set a)) where
  arbitrary :: Gen (DFA (Set a))
  arbitrary = toDFA <$> (arbitrary :: Gen (NFA a))

genNFAString :: NFA a -> Gen String
genNFAString nfa = listOf (elements (Set.toList (alphabet nfa)))

{-
-- Generator for strings accepted by this NFA (if any)
genNFAString :: NFA -> Maybe (Gen String)
-}

validSymbolMap :: Ord a => Set a -> Set Char -> Map Symbol (Set a) -> Bool
validSymbolMap states alphabet map =
  let symbols = Set.insert Epsilon (Set.map Char alphabet)
   in (Set.fromList (Map.keys map) == symbols) && Map.foldr (\x y -> Set.isSubsetOf x states && y) True map

-- Returns true iff the NFA nfa satisfies all validity properties
prop_ValidNFA :: Ord a => NFA a -> Bool
prop_ValidNFA nfa =
  let s = states nfa
      a = alphabet nfa
      tm = transitionMap nfa
      ss = startState nfa
      as = acceptStates nfa
   in Set.member ss s && Set.isSubsetOf as s && (Set.fromList (Map.keys tm) == s)
        && Map.foldr (\x y -> validSymbolMap s a x && y) True tm

-- Returns true iff the DFA dfa satisfies all validity properties
prop_ValidDFA :: DFA a -> Bool
prop_ValidDFA dfa = undefined