primrec
=======

Explore the space of primitive recursive functions. I explain more about why this is worth doing [here](https://github.com/mcoram/primrec/blob/master/raison%20d'e%CC%82tre.md). The work is in the spirit of [inductive programming](http://www.inductive-programming.org/) and was inspired by [MagicHaskeller](http://nautilus.cs.miyazaki-u.ac.jp/~skata/MagicHaskeller.html) and [Incremental Learning in Inductive Programming](http://www.cogsys.wiai.uni-bamberg.de/aaip09/aaip09_submissions/incremental.pdf) and [John Tromp's Lambda Calculus and Combinatory Logic Playground](http://homepages.cwi.nl/~tromp/cl/cl.html).

To get a flavor for what's going on, try running predict-extension.rkt. You'll discover that if you type in "2 3 4 5" then the results tell you that the simplest
primitive recursive function that fits this sequence is the
composition of S with S, which we write as (C11 S S). Here, S stands
for the succesor function S: x -> x+1, and composition is written
using C11 because we want to combine an arity 1 function with an arity
1 function. We write C11 first because the implementation language is
Racket (scheme) and it uses a prefix notation. Essentially (C11 S S) maps x into x+2. 
In the output (C11 S S) is displayed as:

    (#(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26) 3 (C11 S S))

Which displays 3 parts: first the output of applying the function to the integers 0..24,
second the complexity of the function, which is 3 because we simply count the number of
operators to determine the complexity, finally the function itself (written as scheme code).

What do you expect to get if you input "1 2 4"? Did you get it?

The predict-extension.rkt code does this using the results of running pr04.rkt: out/functions.serial
which currently documents all arity 0 functions up to depth 19, arity 1 functions to depth 2 less,
arity 2 functions to depth 3 less, and arity 3 functions to depth 4 less, but if you run pr04.rkt yourself
it will essentially run forever trying to get ever deeper (ok, it'll terminate if it ever gets to 50).
By depth 19 it has discovered 17 functions of arity 0 (0..16 actually), 17867 functions of arity 1, 52342 functions of arity 2, and 14322 functions of arity 3.
By depth 21 the counts are up to 30, 71330, 223609, and 53828 respectively.
So far, the code only considers primitive recursive functions of up-to arity 3.

The basic primitive recursive functions are (see [pr_primitives.rkt](https://github.com/mcoram/primrec/blob/master/pr_primitives.rkt) for details):
*  0 of arity 0
*  S, P11 of arity 1
*  P21, P22 of arity 2
*  P31, P32, P33 of arity 3

where Pij takes i arguments and returns the j'th one (one-indexed).

These are combined using composition (C10, C11, C12, C13, C20, C21, ... C33) and primitive recursion,
which is represented using:
*  R0 (which takes two functions: one of arity 2 and one of arity 0)
*  R1 (which takes two functions: one of arity 3 and one of arity 1)

pr04.rkt tries to be clever about how it explores the space of primitive recursive functions. Specifically,
it builds functions systematically, from lowest complexity on up, and as it goes it keeps track of whether
the functions are observably distinct. Only distinct functions are retained to be reused in the generation of
subsequent, more complicated functions.

Limitations
-----------

To determine if functions are observationally distinct, it has to evaluate them.
* Functions of arity 0 are evaluated.
* Functions of arity 1 are evaluated on 0..24.
* Functions of arity 2 are evaluated on (0..4)^2.
* Functions of arity 3 are evaluated on (0..3)^3.
(This is all configurable in pr04.rkt.)

A limitation of this strategy is that functions could be identical on these sets but differ subsequently. In this case,
the algorithm only keeps the one it encountered first. The second limitation is that you have to evaluate at all.
I allow 5 seconds for the computation to complete; those that don't complete are called "slow" and are recorded in l-slow,
but not used in further exploration.
