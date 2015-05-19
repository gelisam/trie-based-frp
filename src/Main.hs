{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
module Main where

import Data.Maybe
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

-- (commented out, as the true definition is via BehaviorT)
--
--     data Behavior t a = Behavior
--       { currentValue :: a
--       , unBehavior :: ExTrie Behavior t a
--       }

runBehavior :: HasTrie t => Behavior t a -> t -> Behavior t a
runBehavior = runTrie . unBehavior


-- An Event is represented as a lazy trie in which each node holds a number of
-- event occurrences, usually zero. The root node does not have any event
-- occurrence.

-- (commented out, as the true definition is via EventResultT and EventT)
--
--     data EventResult t a = EventResult
--       { eventOccurrences :: [a]
--       , nextEvent :: Event t a
--       } deriving Functor
--     
--     newtype Event t a = Event
--       { unEvent :: ExTrie EventResult t a
--       }

runEvent :: HasTrie t => Event t a -> t -> EventResult t a
runEvent = runTrie . unEvent


-- To understand why we need BehaviorT and EventT, consider a higher-order
-- event, that is, an Event whose occurrences are themselves Events:
-- 
--     Event t (Event t a)
-- 
-- When 't' gets extended, the above becomes
-- 
--     Event (Extend t) (Event t a)
-- 
-- Since the event occurrences still have type 'Event t a', they cannot depend
-- on the new external event which occurs in 'Extend t' but not in 't'. We
-- would like the following instead:
-- 
--     Event (Extend t) (Event (Extend t) a)
-- 
-- In order to express the fact that both 't's should become 'Extend t', the
-- type of event occurrences must be indexed by 't'. This is what EventT does.

data BehaviorT f t a = BehaviorT
  { currentValueT :: f t a
  , unBehaviorT :: ExTrie (BehaviorT f) t a
  }

data EventResultT f t a = EventResultT
  { eventOccurrencesT :: [f t a]
  , nextEventT :: EventT f t a
  }

newtype EventT f t a = EventT
  { unEventT :: ExTrie (EventResultT f) t a
  }

runBehaviorT :: HasTrie t => BehaviorT f t a -> t -> BehaviorT f t a
runBehaviorT = runTrie . unBehaviorT

runEventT :: HasTrie t => EventT f t a -> t -> EventResultT f t a
runEventT = runTrie . unEventT


-- Behavior and Event are special cases of BehaviorT and EventT in which the
-- 't' index is ignored.

data Snd t a = Snd { runSnd :: a }

type Behavior = BehaviorT Snd
type EventResult = EventResultT Snd
type Event = EventT Snd

currentValue :: Behavior t a -> a
currentValue = runSnd . currentValueT

unBehavior :: Behavior t a -> ExTrie Behavior t a
unBehavior = unBehaviorT

eventOccurrences :: EventResult t a -> [a]
eventOccurrences = fmap runSnd . eventOccurrencesT

nextEvent :: EventResult t a -> Event t a
nextEvent = nextEventT

unEvent :: Event t a -> ExTrie EventResult t a
unEvent = unEventT


-- Each time we extend 't', a new external event of type () becomes available.
-- Each such external event can be captured as an 'Event t ()'. To demonstrate
-- the types we have constructed so far, let's create a Behavior whose value is
-- the 'Event t ()' for the most recent external event.

externalEvent :: HasTrie t => (t -> Maybe a) -> Event t a
externalEvent = EventT . mkExTrie
  where
    mkExTrie :: forall t a. HasTrie t
             => (t -> Maybe a)
             -> ExTrie EventResult t a
    mkExTrie p = ExTrie (trie go) (mkExTrie pEx)
      where
        go :: t -> EventResult t a
        go t = EventResultT (occurrences t) (externalEvent p)
        
        occurrences :: t -> [Snd t a]
        occurrences = fmap Snd . maybeToList . p
        
        pEx :: Extend t -> Maybe a
        pEx (Left ()) = Nothing
        pEx (Right t) = p t

lastExternalEvent :: HasTrie t => Event t () -> BehaviorT Event t ()
lastExternalEvent e0 = BehaviorT e0 (mkExTrie e0)
  where
    mkExTrie :: forall t. HasTrie t
             => Event t () -> ExTrie (BehaviorT Event) t ()
    mkExTrie e = ExTrie (trie go) (mkExTrie eEx)
      where
        go :: t -> BehaviorT Event t ()
        go t = BehaviorT e (mkExTrie e)
        
        eEx :: Event (Extend t) ()
        eEx = externalEvent isLast
        
        isLast :: Extend t -> Maybe ()
        isLast (Left ()) = Just ()
        isLast (Right _) = Nothing


main :: IO ()
main = putStrLn "typechecks."
