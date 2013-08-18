* speedup to heuristically reduce burden of slow functions producing stuff like:
  (evaluation-timeout)
  (on-new (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 -1 -1 -1) 17 (C31 P33 (C21 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) S P11) S S) 1191243 -1))
  (evaluation-timeout)
  (on-new (#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 -1 -1 -1) 17 (C31 P33 (C21 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) S P11) S P11) 1191244 -1))
  (evaluation-timeout)
  (on-new (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 -1 -1 -1) 17 (C31 P33 (C21 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) S P11) P11 S) 1191245 -1))
  (evaluation-timeout)
  (on-new (#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 -1 -1 -1) 17 (C31 P33 (C21 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) S P11) P11 P11) 1191246 -1))
  (evaluation-timeout)
  (on-new (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 -1 -1) 17 (C31 P33 (C21 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) P11 S) S S) 1191247 -1))
  (evaluation-timeout)
  (on-new (#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 -1 -1) 17 (C31 P33 (C21 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) P11 S) S P11) 1191248 -1))
  (evaluation-timeout)
  (on-new (#(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 -1 -1) 17 (C31 P33 (C21 (R1 (C23 (R1 (C13 S P31) P11) P31 P31) P11) P11 S) P11 S) 1191249 -1))
  ...

** e.g. could do partial evaluation of (Cij Pik a1 a2 a3) -> ak of arity j
** probably more important, could only call "on-new" in the event of timeout IF the evaluation shows a distinct output among the non-(-1) content

* Use places to parallelize the for loops in pr04

Finished
========
* Confirm/figure a fix for the associativity rule foulup
* Capture potential slow evaluation of arity 0 constructions
* change search strategy to treat slow functions as "novel"
