# Finite Automata and Regular Expression Library
Names: Rahul Chandran, Sean Banko
Pennkeys: rahulrc, sbanko

This library allows you to create perform operations on NFAs, DFAs, and regular expressions.
We have implemented an NFA to DFA conversion algorithm, a DFA minimization algorithm, a regular expression
to NFA conversion algorithm, and an NFA to regular expression conversion algorithm. 

## Module organization

The `src` folder contains all the `.hs` files that implement the algorithms. 
`FA.hs` contains some common functions for NFAs and DFAs. `NFADFAConv.hs` contains the algorithm
for converting an NFA to a DFA. `NFAOperations.hs` contains the union, concatenate, and star operations for 
NFAs (this is needed for regular expression conversion). `NFARegexConv.hs` contains the algorithm
for converting NFAs to regular expressions and vice-versa. `DFAMinimization.hs` contains the algorithm
for DFA minimization.


## Building, running, and testing


The `test` folder contains all the `.hs` files that test our algorithms. It also 
includes `Generators.hs` which has the generators for DFAs and NFAs and the generator
for a string given an DFA or an NFA.

This project compiles with `stack build`. 

You can run the main executable with `stack run`.
You can run the tests with `stack test`. 

Lastly, you can start a REPL with `stack ghci`.
