Why should this project exist?
==============================

I spend a lot of time at work trying to make sense of data. Sometimes
we see a big piece of data and we find that it's not as complicated/noisy as
it might appear because we can detect some regularities in it or lying
underneath the noise. We don't usually know how the data was
generated--perhaps it's from an experiment in which the very goal is to
find out about the biological/scientific mechanisms--but from the
regularities we notice we can get a glimmer of insight about the
mechanism. I want to understand this sort of reasoning more deeply.

The simplest scenario where this sort of thing makes any sense is in
the natural numbers. If I see the number 65536 written down, I immediately think
2^16, because I've seen the powers of two before and because that's a
simple explanation for the appearance of this number. Of course, I don't
know that the mechanism that led to that number being written 
down involved multiplication by 2, as opposed to, say 655*100+6^2, but
because the former explanation exists I think of 65536 as a simpler
number than, say, 55690 (the last 4 digits were chosen by random.org;
the point would be clearer if the last digit wasn't a 0, but oh well).

Anyway, the primitive recursive functions are a classical and simple
system for writing the natural numbers and functions of them, so
systematically exploring the kinds of functions that are simple under
that system seems important.

So far I have discovered, for example, that 15 is simpler than
13 and 14 in the sense that I can write a primitive recursive expression for
15 that involves only 17 symbols, but 13 and 14 require 18 and 19 respectively.
I have also "discovered" a lot of sequences of
integers that appear (with proper histories and significance) in the Online
Encyclopedia of Integer Sequences and some that don't, but maybe
should. (See [notes.txt](https://github.com/mcoram/primrec/blob/master/notes.txt)
for more details)

