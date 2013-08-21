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

Is that intuition valid? Curiously, it turns out that 65536 is first constructed
as a standard-order primitive recursive function with 22 symbols
by applying the function f:i->(i+2)^(2^(i+1)) to 2; where f is implemented
by repeated squaring, and where squaring is implemented via an algorithm that maps i via:

   	      u=0
	      For k from 0 to i-1: u=2*k+u+1
	      return u

So the simplest construction of 65536 is via (2+2) squared (2+1) times. So 65536 is indeed simple, but there's an even "simpler" way to understand it.

Anyway, the primitive recursive functions are a classical and simple
system for writing the natural numbers and functions of them, so
systematically exploring the kinds of functions that are simple under
that system seems important.

So far I have discovered, for example, that 15 is simpler than
13 and 14 in the sense that I can write a primitive recursive expression for
15 that involves only 17 symbols, but 13 and 14 require 18 and 19 respectively. Similarly, 65536 is simpler, in this sense, than 44. In all there are 125 non-negative integers accessible with 22 primitive recursive symbols or less: (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 45 46 47 48 49 52 53 54 55 56 57 60 63 64 65 66 67 68 76 78 79 80 81 82 83 94 95 100 101 102 108 119 120 121 135 136 153 156 168 183 210 223 230 231 232 255 256 289 383 435 436 512 1539 1540 1541 2048 2278 2279 6143 6561 7259 7260 9453 12288 24575 26796 53248 65536 98303 131071 131072 557055 1186570 25165823 26357430 50331648 402653184 8589934591 68719476735 141733920768 22539988369407) 

I have also "discovered" a lot of sequences of
integers that appear (with proper histories and significance) in the Online
Encyclopedia of Integer Sequences and some that don't, but maybe
should. (See [notes.txt](https://github.com/mcoram/primrec/blob/master/notes.txt)
for more details)

