{-# LANGUAGE TypeOperators #-}
module Main where

import Data.MemoTrie


-- Behaviors and Events are both represented as lazy tries containing every
-- possible future. At each node of a trie, a value of type 'Maybe t'
-- determines which of many possible subtrees will be taken.
-- 
-- A value of the form 'Just t' identifies which of several possible external
-- events (not to be confused with Event or event occurrences) has occurred,
-- whereas the value 'Nothing' indicates that a new external event may now
-- occur. In that branch of the trie, subtrees will now be selected by a value
-- of type 'Maybe (Either () t)' instead of 'Maybe t'.

type Extend t = Either () t

data ExTrie f t a = ExTrie
  { normalTrie :: t :->: f t a
  , extendedTrie :: ExTrie f (Extend t) a
  }

runTrie :: HasTrie t => ExTrie f t a -> t -> f t a
runTrie = untrie . normalTrie


-- A Behavior is represented as a lazy trie in which each node holds the
-- value of the behavior at that time, including the root node.

data Behavior t a = Behavior
  { currentValue :: a
  , unBehavior :: ExTrie Behavior t a
  }

runBehavior :: HasTrie t => Behavior t a -> t -> Behavior t a
runBehavior = runTrie . unBehavior


-- An Event is represented as a lazy trie in which each node holds a number of
-- event occurrences, usually zero. The root node does not have any event
-- occurrence.

data EventResult t a = EventResult
  { eventOccurrences :: [a]
  , nextEvent :: Event t a
  }

newtype Event t a = Event
  { unEvent :: ExTrie EventResult t a
  }

runEvent :: HasTrie t => Event t a -> t -> EventResult t a
runEvent = runTrie . unEvent


main :: IO ()
main = putStrLn "typechecks."
