There's this algorithm for finding fixed points in lattices, (see chapter 5.10 in [spa](https://cs.au.dk/~amoeller/spa/spa.pdf)).
It basically just applies a bunch of transfer functions in a nondeterministic order until a fixedpoint is hit.
This repo is an attempt at writing this algorithm to use multiple threads, simply by just having the threads executing the transfer functions.
We don't need to do any funky scheduling stuff, since order doesn't matter (but there may be optimisations that come from scheduling).
