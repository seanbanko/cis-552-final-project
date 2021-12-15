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

-- creates a transition map that, for each state, maps each symbol to the empty set 
emptyTransitionMapNFA :: Ord a => Set a -> Set Char -> Map a (Map Symbol (Set a))
emptyTransitionMapNFA states alphabet =
    Map.fromList (zip (Set.toList states) (repeat (Map.fromList (zip (Set.toList symbols) (repeat Set.empty)))))
    where symbols = Set.insert Epsilon (Set.map FA.Char alphabet)

-- smart constructor for RegExp.Char that converts Char sets to nested Alts of singletons
char :: RegExp -> RegExp
char (RegExp.Char cs) = foldr (alt . RegExp.Char . Set.singleton) RegExp.Void cs
char r = r

-- TODO not sure how to properly handle instantiating the transntion map. does it need to be total always?
toNFA :: RegExp -> NFA Int
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

type GNFA a = FA a (Map RegExp (Set a))

-- creates a transition map for a GNFA mapping every state to every other state
-- except no mapping from the new accepting state or to the new start state
emptyTransitionMapGNFA :: Ord a => Set a -> a -> a -> Map a (Map RegExp (Set a))
emptyTransitionMapGNFA qs q0 qf =
    Map.fromList (zip (Set.toList (Set.delete qf qs)) (repeat (Map.singleton RegExp.Void (Set.delete q0 qs))))

convertTransitions :: Ord a => DFA a -> a -> a -> Map a (Map RegExp (Set a))
convertTransitions d q0 qf =
        -- converts the existing map to a map with RegExp labels and sets of result states
    let tm = Map.map (Map.mapKeys (RegExp.Char . Set.singleton) . Map.map Set.singleton) (transitionMap d)
        -- adds epsilon (empty RegExp) transitions from the new start state to the original start state
        tm' = Map.insert q0 (Map.singleton RegExp.Empty (Set.singleton (startState d))) tm
        -- adds epsilon (empty RegExp) transitions from the original accept states to the new accept state
        tm'' = foldr (Map.adjust (Map.insertWith Set.union RegExp.Empty (Set.singleton qf))) tm' (acceptStates d)
        -- adds Void transitions between any states that do not have an arrow connecting them
        tm''' = Map.unionWith Map.union tm'' (emptyTransitionMapGNFA (states d) q0 qf)
    in tm'''

toGNFA :: DFA Int -> GNFA Int
toGNFA d@(F qs sigma tm q0 fs) = 
    let sigma' = sigma
        q0' = Set.findMax qs + 1
        qf' = q0' + 1
        fs' = Set.singleton qf'
        qs' = Set.unions [qs, Set.singleton q0', fs']
        tm' = convertTransitions d q0' qf'
     in F qs' sigma' tm' q0' fs'

convert :: Ord a => GNFA a -> RegExp
convert g
    | Set.size (states g) == 2 = 
        case Map.keys (transitionMap g ! startState g) of
            [r] -> r
            _ -> error "problem"
    | otherwise = undefined
    


toRegExp :: NFA a -> RegExp
toRegExp = undefined

d5 :: DFA Int
d5 =
  let s = Set.fromList [0, 1, 2, 3, 4, 5]
      a = Set.fromList ['0', '1']
      q0Map = Map.fromList [('0', 1), ('1', 2)]
      q1Map = Map.fromList [('0', 3), ('1', 4)]
      q2Map = Map.fromList [('0', 4), ('1', 3)]
      q3Map = Map.fromList [('0', 5), ('1', 5)]
      q4Map = Map.fromList [('0', 5), ('1', 5)]
      q5Map = Map.fromList [('0', 5), ('1', 5)]
      tm = Map.fromList [(0, q0Map), (1, q1Map), (2, q2Map), (3, q3Map), (4, q4Map), (5, q5Map)]
      ss = 0
      as = Set.fromList [1, 2, 5]
   in F s a tm ss as



















