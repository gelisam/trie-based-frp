# Trie-based FRP

Exploring a part of the FRP design space in which events and behaviors are implemented via [tries](http://en.wikipedia.org/wiki/Trie).

## Tries vs Functions

In a post named "[homemade FRP](http://gelisam.blogspot.com/2014/07/homemade-frp-study-in-following-types.html)", I wrote a naïve FRP implementation based on simple functions. When it turned out that this implementation strategy could lead to exponentially-bad performance, I converted those functions into tries; the same function, but represented as a value instead of a computation.

In order to explain the difference between the two representations, I must define a few terms. Since Haskell is a pure language, if I call the same function twice with the same argument, the two calls will return the same value. I will say that two such values are "equivalent". Similarly, if I access the first component of a pair twice, I will have accessed the same value twice. I will say that two such values are "identical". Two pieces of code who hold identical values are said to "share" that value.

Sharing is important for performance. Since Haskell is lazy, any value might be represented as a thunk which will be evaluated the first time the value is forced. If I hold two equivalent values and I force them both, I might end up evaluating two separate thunks. Whereas if was to force two identical values, they would be sharing the same thunk, so forcing the first value will evaluate the thunk, and forcing the second value will use the already-evaluated result.

The reason I ended up with exponentially-bad performance while using functions is that calling those functions caused sharing to be lost. Two part of my event network were sharing an identical value, both called the same function on that value, but ended up with equivalent results instead of identical results. In a mutually-recursive setting, this meant that the number of computations I had to perform doubled after every step.

By using tries, sharing is preserved because instead of calling the same function on two identical values, the two parts of my event graph are now accessing the same field of that value, and therefore end up with identical results.

## Motivation

There is a large gap between a naïve and a practical implementation of an FRP library. Look at the source for [reactive-banana](http://hackage.haskell.org/package/reactive-banana) or [sodium](http://hackage.haskell.org/package/sodium), and you will see a surprising number of calls to `unsafePerformIO` and other hacks to increase sharing and ensure prompt garbage-collection. Heinrich Apfelmus, the author of [reactive-banana](http://hackage.haskell.org/package/reactive-banana), lists a number of important [issues when implementing an FRP library](http://www.reddit.com/r/haskell/comments/34z9it/write_you_a_frp_library_for_great_good_200_line/cr01lrp).

Due to this gap, I assumed that while tries allowed me to fix the performance issue of my naïve implementation, real FRP libraries must have good reasons not to use them. However, the few discussions I have had with Heinrich Apfelmus over the internet ([1](https://github.com/HeinrichApfelmus/reactive-banana/issues/79#issuecomment-98209246), [2](http://www.reddit.com/r/haskell/comments/34z9it/write_you_a_frp_library_for_great_good_200_line/cr064t1)) indicated that

1. he still had difficulties getting sharing to work with recursive definitions, and
1. he did not know about the technique of implementing FRP using tries.

Since I specifically introduced tries in order to make sure recursive definitions preserved sharing, I thought it would be worthwhile to add some comments, upload my implementation, and offer it for consideration.

## Goals

The dream scenario would be for Heinrich to look at my code, learning a new trick (is there any Haskell trick he doesn't know already?), and deciding to use it to improve sharing in reactive-banana.

An okay scenario would be for someone to find a flaw in my design which makes it definitely unsuitable for a real-world FRP library. Heinrich already found a [flaw](http://www.reddit.com/r/haskell/comments/34z9it/write_you_a_frp_library_for_great_good_200_line/cr4k0fz) in the original design, namely that new events could not be added to an event network after it had been constructed. I have now written a more complicated version which does not have this flaw, let's see what else will come up.

The worse scenario would be to discover that tries are a fine way to implement an FRP library, that there are advantages over other implementations, but also disadvantages, and so I should package my code as a library and let users choose which set of tradeoffs they want. I say this would be the worse scenario because there are already [so many FRP implementations](https://github.com/gelisam/frp-zoo#readme), does the world really need yet another one?
