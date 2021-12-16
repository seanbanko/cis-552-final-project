module NFADFAConvTests where

import NFARegexConv
import FA
import NFADFAConv
import NFAOperations
import RegExp

import qualified Data.List as List
import Data.Map (Map, (!), (!?))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple
import Test.HUnit

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

test_convertTransitions :: Test
test_convertTransitions =
  "convert transitions tests"
    ~: TestList
      [ convertTransitions d5 6 7 ~?=
          Map.fromList [
              (0,Map.fromList [(0,Void),(1,RegExp.Char (Set.fromList "0")),(2,RegExp.Char (Set.fromList "1")),(3,Void),(4,Void),(5,Void)]),
              (1,Map.fromList [(0,Void),(1,Void),(2,Void),(3,RegExp.Char (Set.fromList "0")),(4,RegExp.Char (Set.fromList "1")),(5,Void),(7,Empty)]),
              (2,Map.fromList [(0,Void),(1,Void),(2,Void),(3,RegExp.Char (Set.fromList "1")),(4,RegExp.Char (Set.fromList "0")),(5,Void),(7,Empty)]),
              (3,Map.fromList [(0,Void),(1,Void),(2,Void),(3,Void),(4,Void),(5,Alt (RegExp.Char (Set.fromList "1")) (RegExp.Char (Set.fromList "0")))]),
              (4,Map.fromList [(0,Void),(1,Void),(2,Void),(3,Void),(4,Void),(5,Alt (RegExp.Char (Set.fromList "1")) (RegExp.Char (Set.fromList "0")))]),
              (5,Map.fromList [(0,Void),(1,Void),(2,Void),(3,Void),(4,Void),(5,Alt (RegExp.Char (Set.fromList "1")) (RegExp.Char (Set.fromList "0"))),(7,Empty)]),
              (6,Map.fromList [(0,Empty)])
          ]
      ]

test_convert :: Test
test_convert =
  "convert transitions tests"
    ~: TestList
      [ convert (toGNFA d5) ~?=
            Alt
                (Append 
                    (Alt 
                        (Append 
                            (Alt 
                                (Append 
                                    (RegExp.Char (Set.fromList "1")) 
                                    (RegExp.Char (Set.fromList "0"))
                                ) 
                                (Append 
                                    (RegExp.Char (Set.fromList "0")) 
                                    (RegExp.Char (Set.fromList "1"))
                                )
                            ) 
                            (Alt 
                                (RegExp.Char (Set.fromList "1")) 
                                (RegExp.Char (Set.fromList "0"))
                            )
                        ) 
                        (Append 
                            (Alt 
                                (Append 
                                    (RegExp.Char (Set.fromList "1")) 
                                    (RegExp.Char (Set.fromList "1"))
                                )
                                (Append 
                                    (RegExp.Char (Set.fromList "0")) 
                                    (RegExp.Char (Set.fromList "0"))
                                )
                            ) 
                            (Alt 
                                (RegExp.Char (Set.fromList "1")) 
                                (RegExp.Char (Set.fromList "0"))
                            )
                        )
                    ) 
                    (Star 
                        (Alt 
                            (RegExp.Char (Set.fromList "1")) 
                            (RegExp.Char (Set.fromList "0"))
                        )
                    )
                )
                (Alt 
                    (RegExp.Char (Set.fromList "1")) 
                    (RegExp.Char (Set.fromList "0"))
                )
      ]