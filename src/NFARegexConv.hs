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

-- Creates a transition map for a GNFA that, for each state, maps each state to Void
voidTransitionMapGNFA :: Ord a => Set a -> a -> a -> Map a (Map a RegExp)
voidTransitionMapGNFA qs q0 qf = 
    let qis = Set.toList (Set.delete qf qs)
        qjs = Set.toList (Set.delete q0 qs)
    in Map.fromList (zip qis (repeat (Map.fromList (zip qjs (repeat RegExp.Void)))))

-- Creates a copy of the transition map that contains keys and values for each pair of states
makeTotalTransitionMapGNFA :: Ord a => Set a -> a -> a -> Map a (Map a RegExp) -> Map a (Map a RegExp)
makeTotalTransitionMapGNFA qs q0 qf delta = Map.unionWith Map.union delta (voidTransitionMapGNFA qs q0 qf)

-- Inverts the transition map of a DFA for a GNFA
invert :: Ord a => Map a (Map Char a) -> Map a (Map a RegExp)
invert = Map.map (Map.fromListWith alt . map (\(c, q) -> (q, char c)) . Map.toList)

-- Adds epsilon transitions from *srcs* to *tgt* for a GNFA
addEpsilonTransitionsGNFA :: Ord a => Set a -> a -> Map a (Map a RegExp) -> Map a (Map a RegExp)
addEpsilonTransitionsGNFA srcs tgt delta = foldr (Map.adjust (Map.insert tgt RegExp.Empty)) delta srcs

-- Creates a transition map for the Generalized NFA of a DFA, given the new start state and final state
generalizeTransitionMap :: Ord a => DFA a -> a -> a -> Map a (Map a RegExp) 
generalizeTransitionMap d q0 qf = 
    let tm    = invert (transitionMap d)
        q0Map = Map.singleton (startState d) RegExp.Empty
        tm'   = Map.insert q0 q0Map tm
        tm''  = addEpsilonTransitionsGNFA (acceptStates d) qf tm'
        tm''' = makeTotalTransitionMapGNFA (Set.unions [states d, Set.singleton q0, Set.singleton qf]) q0 qf tm''
     in tm'''

toGNFA :: DFA Int -> GNFA Int
toGNFA d@(F qs sigma tm q0 fs) = 
    let sigma' = sigma
        q0' = Set.findMax qs + 1
        qf' = q0' + 1
        fs' = Set.singleton qf'
        qs' = Set.unions [qs, Set.singleton q0', fs']
        tm' = generalizeTransitionMap d q0' qf'
     in F qs' sigma' tm' q0' fs'

-- TODO make this safe?
convert :: Ord a => GNFA a -> RegExp
convert g
    | Set.size (states g) == 2  = transitionMap g ! startState g ! Set.elemAt 0 (acceptStates g)
    | Set.size (states g) < 2   = error "A GNFA cannot have less than 2 states"
    | otherwise                 = convert g' where g' = rip g

rip :: Eq a => Ord a => GNFA a -> GNFA a
rip g = case List.find (\q -> (q /= startState g) && notElem q (acceptStates g)) (Map.keys (transitionMap g)) of
    Just qrip -> g {states = Set.delete qrip (states g), transitionMap = ripTransitions g qrip}
    _ -> error "No valid states found to rip"

ripTransitions :: Ord a => GNFA a -> a -> Map a (Map a RegExp) 
ripTransitions g qrip =
    let q'    = Set.delete qrip (states g)
        qis   = Set.delete (Set.elemAt 0 (acceptStates g)) q'
        qjs   = Set.delete (startState g) q'
        pairs = Set.toList (Set.cartesianProduct qis qjs)
        tm    = foldr (\(qi, qj) mp -> Map.adjust (Map.insert qj (ripRegExp g qi qj qrip)) qi mp) (transitionMap g) pairs
        tm'   = Map.delete qrip (Map.map (Map.delete qrip) tm)
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

