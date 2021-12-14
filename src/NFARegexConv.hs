module NFARegexConv where

import FA
import NFAOperations
import RegExp

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

toNFA :: RegExp -> NFA Int

-- creates a transition map that, for each state, maps each symbol to the empty set 
emptyTransitionMapNFA :: Ord a => Set a -> Set Char -> Map a (Map Symbol (Set a))
emptyTransitionMapNFA states alphabet =
    Map.fromList (zip (Set.toList states) (repeat (Map.fromList (zip (Set.toList symbols) (repeat Set.empty)))))
    where symbols = Set.insert Epsilon (Set.map FA.Char alphabet)

-- smart constructor for RegExp.Char that converts Char sets to nested Alts of singletons
-- uses alt's smart constructor to eliminate the Void base case
char :: RegExp -> RegExp
char (RegExp.Char cs) = foldr (alt . RegExp.Char . Set.singleton) RegExp.Void cs
char r = r

-- TODO not sure how to properly handle instantiating the transntion map. does it need to be total always?
toNFA r@(RegExp.Char cset)
    | Set.size cset > 1 = toNFA (char r) 
    | otherwise = 
    let s = Set.fromList [1, 2]
        a = Set.singleton (Set.elemAt 0 cset)
        tm = Map.insertWith Map.union ss (Map.fromList [(FA.Char (Set.elemAt 0 cset), Set.singleton 2)]) (emptyTransitionMapNFA s a)
        ss = 1
        as = Set.singleton 2
     in F s a tm ss as
toNFA Empty = 
    let s = Set.singleton 1
        a = Set.empty 
        tm = emptyTransitionMapNFA s a
        ss = 1
        as = Set.singleton ss
     in F s a tm ss as
toNFA Void = 
    let qs = Set.singleton 1
        sigma = Set.empty 
        tm = emptyTransitionMapNFA qs sigma
        q0 = 1
        fs = Set.empty
     in F qs sigma tm q0 fs
toNFA (Alt r1 r2)       = NFAOperations.union (toNFA r1) (toNFA r2)
toNFA (Append r1 r2)    = NFAOperations.concatenate (toNFA r1) (toNFA r2)
toNFA (Star r)          = NFAOperations.star (toNFA r) 

-- todo change the type of regexp char to just take a single char
-- toGNFA :: DFA a -> (NFA a or GNFA a)
toRegExp :: NFA a -> RegExp
toRegExp = undefined