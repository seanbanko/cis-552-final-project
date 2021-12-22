CIS 552 Final Project, Fall 2021

Project name: Regular Language Library
Github repo: https://github.com/cis552/project_t21-project/
Group members: Rahul Chandran, Sean Banko
Mentor: Harry Goldstein

NOTE: after CIS 552 is over, we will be removing all repositories from github. If you want to make
your project public, you should move it to your own github account.

Comments:

# Total Score 96/100

## Proposal          5/5
## CP #1            10/10
## CP #2             9/10

## Correctness      25/25
+ Everything seems correct

## Design           28/30 
* Modularity
  + Good module structure

* Types
  + FA type is a nice abstraction over DFAs and NFAs

* FP
  - Algorithms are pretty much one-to-one with imperative code; you could have done a bit more to
    functionalize some of these ideas

* Functors, Monads, etc.
  + I appreciate the `fmap` operations to get around issues with `Ord`

* Abstraction
  + The generalized NFA was a nice fix for a tricky algorithm

## Testing          15/15
+ Glad that you caught some real bugs with your tests!
+ Nice job extracting statstics when generators were difficult to write

## Style             4/5
- Lines are pretty long, some nesting is a bit too deep
