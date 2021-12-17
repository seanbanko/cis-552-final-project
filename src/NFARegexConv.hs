module NFARegexConv where

import FA
import NFADFAConv
import DFAMinimization
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

toNFA :: RegExp -> NFA Int
toNFA r@(RegExp.Char cset)
    | Set.size cset > 1 = toNFA (simplifyChar r) 
    | Set.size cset == 0 = toNFA RegExp.Void
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
invertDFA :: Ord a => Map a (Map Char a) -> Map a (Map a RegExp)
invertDFA = Map.map (Map.fromListWith alt . map (\(c, q) -> (q, char c)) . Map.toList)

-- Inverts the transition map of a NFA for a GNFA
invertNFA :: Ord a => Map a (Map Symbol (Set a)) -> Map a (Map a RegExp)
invertNFA = Map.map (Map.fromListWith alt . pairs)
    where
        pairs :: Map Symbol (Set a) -> [(a, RegExp)]
        pairs mp = [(q, g s) | (s, qs) <- Map.toList mp, q <- Set.toList qs]
        g :: Symbol -> RegExp
        g s = case s of 
            FA.Char c -> char c
            Epsilon -> RegExp.Empty

-- Adds epsilon transitions from *srcs* to *tgt* for a GNFA
addEpsilonTransitionsGNFA :: Ord a => Set a -> a -> Map a (Map a RegExp) -> Map a (Map a RegExp)
addEpsilonTransitionsGNFA srcs tgt delta = foldr (Map.adjust (Map.insert tgt RegExp.Empty)) delta srcs

-- Creates a transition map for the Generalized NFA of a DFA, given the new start state and final state
generalizeTransitionMapDFA :: Ord a => DFA a -> a -> a -> Map a (Map a RegExp) 
generalizeTransitionMapDFA d q0 qf = 
    let tm    = invertDFA (transitionMap d)
        q0Map = Map.singleton (startState d) RegExp.Empty
        tm'   = Map.insert q0 q0Map tm
        tm''  = addEpsilonTransitionsGNFA (acceptStates d) qf tm'
        tm''' = makeTotalTransitionMapGNFA (Set.unions [states d, Set.singleton q0, Set.singleton qf]) q0 qf tm''
     in tm'''

-- Creates a transition map for the Generalized NFA of an NFA, given the new start state and final state
generalizeTransitionMapNFA :: Ord a => NFA a -> a -> a -> Map a (Map a RegExp) 
generalizeTransitionMapNFA d q0 qf = 
    let tm    = invertNFA (transitionMap d)
        q0Map = Map.singleton (startState d) RegExp.Empty
        tm'   = Map.insert q0 q0Map tm
        tm''  = addEpsilonTransitionsGNFA (acceptStates d) qf tm'
        tm''' = makeTotalTransitionMapGNFA (Set.unions [states d, Set.singleton q0, Set.singleton qf]) q0 qf tm''
     in tm'''

-- Converts a DFA to a Generalized NFA
toGNFADFA :: DFA Int -> GNFA Int
toGNFADFA d@(F qs sigma tm q0 fs) = 
    let sigma' = sigma
        q0' = Set.findMax qs + 1
        qf' = q0' + 1
        fs' = Set.singleton qf'
        qs' = Set.unions [qs, Set.singleton q0', fs']
        tm' = generalizeTransitionMapDFA d q0' qf'
     in F qs' sigma' tm' q0' fs'

-- Converts an NFA to a Generalized NFA
toGNFANFA :: NFA Int -> GNFA Int
toGNFANFA n@(F qs sigma tm q0 fs) = 
    let sigma' = sigma
        q0' = Set.findMax qs + 1
        qf' = q0' + 1
        fs' = Set.singleton qf'
        qs' = Set.unions [qs, Set.singleton q0', fs']
        tm' = generalizeTransitionMapNFA n q0' qf'
     in F qs' sigma' tm' q0' fs'

convert :: Ord a => GNFA a -> RegExp
convert g
    | Set.size (states g) == 2  = transitionMap g ! startState g ! Set.elemAt 0 (acceptStates g)
    | Set.size (states g) < 2   = error "A GNFA cannot have less than 2 states"
    | otherwise                 = 
        let g' = rip g in
            if Set.size (states g') == Set.size (states g) then error "rip did not remove a state" else convert g' 

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

toDFAInt :: Ord a => DFA (Set a) -> DFA Int 
toDFAInt d@(F qs sigma delta q0 fs) = 
    let mp = Map.fromList (zip (Set.toList qs) [1..]) in
        fmapDFA (mp !) d

toNFAInt :: Ord a => NFA a -> NFA Int 
toNFAInt n@(F qs sigma delta q0 fs) = 
    let mp = Map.fromList (zip (Set.toList qs) [1..]) in
        fmapNFA (mp !) n

-- toRegExp :: Ord a => NFA a -> RegExp
-- toRegExp = convert . toGNFADFA . toDFAInt . minimizeDFA . toDFA 

toRegExp :: Ord a => NFA a -> RegExp
toRegExp = convert . toGNFANFA . toNFAInt
