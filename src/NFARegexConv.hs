module NFARegexConv where

import FA
import NFADFAConv
import NFAOperations
import RegExp

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit
import Data.Tuple

-- Smart constructor for RegExp.Char for a single char
char :: Char -> RegExp
char = RegExp.Char . Set.singleton

-- Simplifier for RegExp.Char that converts RegExp.Char sets to Alts of singletons
simplifyChar :: RegExp -> RegExp
simplifyChar (RegExp.Char cs) = foldr (alt . char) RegExp.Void cs
simplifyChar r = r

-- Map.unionWith Map.union tm'' (voidTransitionMapGNFA (Set.unions [states d, Set.singleton q0, Set.singleton qf]) q0 qf)
-- TODO not sure how to properly handle instantiating the transition map. does it need to be total always?
toNFA :: RegExp -> NFA Int
toNFA r@(RegExp.Char cset)
    | Set.size cset > 1 = toNFA (simplifyChar r) 
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

type GNFA a = FA a (Map a RegExp)

-- creates a transition map for a GNFA mapping each state to each other state with a Void arrow
voidTransitionMapGNFA :: Ord a => Set a -> a -> a -> Map a (Map a RegExp)
voidTransitionMapGNFA qs q0 qf = 
    Map.fromList (zip (Set.toList (Set.delete qf qs)) (repeat (Map.fromList (zip (Set.toList (Set.delete q0 qs)) (repeat RegExp.Void)))))

convertTransitions :: Ord a => DFA a -> a -> a -> Map a (Map a RegExp) 
convertTransitions d q0 qf = 
              -- inverts the keys and values of the dfa's transition map
    let tm    = Map.map (Map.fromListWith alt . map (\(c, q) -> (q, char c)) . Map.toList) (transitionMap d)
              -- adds an epsilon (empty RegExp) transition from the new start state to the original start state
        tm'   = Map.insert q0 (Map.singleton (startState d) RegExp.Empty) tm
              -- adds epsilon (empty RegExp) transitions from the original accept states to the new accept state
        tm''  = foldr (Map.adjust (Map.insert qf RegExp.Empty)) tm' (acceptStates d)
              -- adds Void transitions between any states that do not have an arrow connecting them
        tm''' = Map.unionWith Map.union tm'' (voidTransitionMapGNFA (Set.unions [states d, Set.singleton q0, Set.singleton qf]) q0 qf)
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

-- TODO make this safe
convert :: Ord a => GNFA a -> RegExp
convert g
    | Set.size (states g) == 2  = transitionMap g ! startState g ! Set.elemAt 0 (acceptStates g)
    | Set.size (states g) < 2   = error "A GNFA cannot have less than 2 states"
    | otherwise                 = convert g' where g' = rip g

rip :: Eq a => Ord a => GNFA a -> GNFA a
rip g = case List.find (\q -> (q /= startState g) && notElem q (acceptStates g)) (Map.keys (transitionMap g)) of
    Just qrip -> g {states = Set.delete qrip (states g), transitionMap = ripTransitions g qrip}
    _ -> error "problem"

ripTransitions :: Ord a => GNFA a -> a -> Map a (Map a RegExp) 
ripTransitions g qrip =
    let q'    = Set.delete qrip (states g)
        qis   = Set.delete (Set.elemAt 0 (acceptStates g)) q'
        qjs   = Set.delete (startState g) q'
        pairs = Set.toList (Set.cartesianProduct qis qjs)
        tm = foldr (\(qi, qj) mp -> Map.adjust (Map.insert qj (ripRegExp g qi qj qrip)) qi mp) (transitionMap g) pairs
        tm' = Map.delete qrip (Map.map (Map.delete qrip) tm)
     in tm'

ripRegExp :: Ord a => GNFA a -> a -> a -> a -> RegExp
ripRegExp g qi qj qrip = alt (append r1 (append (RegExp.star r2) r3)) r4 
    where
        r1 = transitionMap g ! qi ! qrip 
        r2 = transitionMap g ! qrip ! qrip
        r3 = transitionMap g ! qrip ! qj
        r4 = transitionMap g ! qi ! qj


-- toDFAInt :: DFA a -> DFA Int 
-- toDFAInt d@(F qs sigma tm q0 fs) = 
--     let sigma' = sigma
--         q0' = Set.findMax qs + 1
--         qf' = q0' + 1
--         fs' = Set.singleton qf'
--         qs' = Set.unions [qs, Set.singleton q0', fs']
--         tm' = convertTransitions d q0' qf'
--      in F qs' sigma' tm' q0' fs'


-- TODO toNFA currently has the wrong output type
toRegExp :: NFA Int -> RegExp
toRegExp n = undefined -- convert (toGNFA (NFADFAConv.toDFA n))

