{-# LANGUAGE DeriveFunctor, ScopedTypeVariables, StandaloneDeriving, TypeOperators #-}
module Main where

import Control.Arrow
import Data.Constraint
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

mkExTrie :: HasTrie t
         => (t -> f t a)
         -> ExTrie f (Extend t) a
         -> ExTrie f t a
mkExTrie = ExTrie . trie

runTrie :: HasTrie t => ExTrie f t a -> t -> f t a
runTrie = untrie . normalTrie


-- A Behavior is represented as a lazy trie in which each node holds the
-- value of the behavior at that time, including the root node.

-- (commented out, as the true definition is via BehaviorT)
--
--    data Behavior t a = Behavior
--      { currentValue :: a
--      , unBehavior :: ExTrie Behavior t a
--      }
--    
--    mkBehavior :: HasTrie t
--               => a
--               -> (t -> Behavior t a)
--               -> Behavior (Extend t) a
--               -> Behavior t a
--    mkBehavior fx f = Behavior fx . mkExTrie f . unBehavior

runBehavior :: HasTrie t => Behavior t a -> t -> Behavior t a
runBehavior = runTrie . unBehavior


-- An Event is represented as a lazy trie in which each node holds a number of
-- event occurrences, usually zero. The root node does not have any event
-- occurrence.

-- (commented out, as the true definition is via EventResultT and EventT)
--
--    data EventResult t a = EventResult
--      { eventOccurrences :: [a]
--      , nextEvent :: Event t a
--      }
--    
--    newtype Event t a = Event
--      { unEvent :: ExTrie EventResult t a
--      }
--    
--    mkEvent :: forall t a. HasTrie t
--            => (t -> ([a], Event t a))
--            -> Event (Extend t) a
--            -> Event t a
--    mkEvent f = Event . mkExTrie f' . unEvent
--      where
--        f' :: t -> EventResult t a
--        f' = uncurry EventResult . f

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

mkBehaviorT :: HasTrie t
            => f t a
            -> (t -> BehaviorT f t a)
            -> BehaviorT f (Extend t) a
            -> BehaviorT f t a
mkBehaviorT fx f = BehaviorT fx . mkExTrie f . unBehaviorT

mkEventT :: HasTrie t
         => (t -> EventResultT f t a)
         -> EventT f (Extend t) a
         -> EventT f t a
mkEventT f = EventT . mkExTrie f . unEventT

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

eventOccurrences :: EventResult t a -> [a]
eventOccurrences = fmap runSnd . eventOccurrencesT

nextEvent :: EventResult t a -> Event t a
nextEvent = nextEventT

unBehavior :: Behavior t a -> ExTrie Behavior t a
unBehavior = unBehaviorT

unEvent :: Event t a -> ExTrie EventResult t a
unEvent = unEventT

mkBehavior :: HasTrie t
           => a
           -> (t -> Behavior t a)
           -> Behavior (Extend t) a
           -> Behavior t a
mkBehavior fx f = BehaviorT (Snd fx) . mkExTrie f . unBehaviorT

mkEvent :: forall t a. HasTrie t
        => (t -> ([a], Event t a))
        -> Event (Extend t) a
        -> Event t a
mkEvent f = EventT . mkExTrie f' . unEventT
  where
    f' :: t -> EventResult t a
    f' = uncurry EventResultT . first (fmap Snd) . f


-- Each time we extend 't', a new external event of type () becomes available.
-- Each such external event can be captured as an 'Event t ()'. To demonstrate
-- the types we have constructed so far, let's create a Behavior whose value is
-- the 'Event t ()' for the most recent external event.

externalEvent :: HasTrie t => (t -> Maybe a) -> Event t a
externalEvent = go
  where
    go :: forall t a. HasTrie t
        => (t -> Maybe a)
        -> Event t a
    go p = r
      where
        r :: Event t a
        r = mkEvent f (go pEx)
        
        f :: t -> ([a], Event t a)
        f t = (occurrences t, r)
        
        occurrences :: t -> [a]
        occurrences = maybeToList . p
        
        pEx :: Extend t -> Maybe a
        pEx (Left ()) = Nothing
        pEx (Right t) = p t

lastExternalEvent :: HasTrie t => Event t () -> BehaviorT Event t ()
lastExternalEvent = go
  where
    go :: forall t. HasTrie t
       => Event t ()
       -> BehaviorT Event t ()
    go e = r
      where
        r :: BehaviorT Event t ()
        r = mkBehaviorT e (const r) (go eEx)
        
        eEx :: Event (Extend t) ()
        eEx = externalEvent isLast
        
        isLast :: Extend t -> Maybe ()
        isLast (Left ()) = Just ()
        isLast (Right _) = Nothing


main :: IO ()
main = putStrLn "typechecks."


-- Feel free to skip the remainder of this file, it's both technical and
-- relatively unimportant.


-- We would like Functor instances for everything, but the fact that 't'
-- changes makes the condition on 'f' a bit too complicated for a regular
-- constraint. Instead, we specify our requirements using Data.Constraint.

class FunctorFT f where
    functorFT :: HasTrie t :- Functor (f t)

fmapFT :: forall f t a b. (FunctorFT f, HasTrie t)
       => (a -> b) -> f t a -> f t b
fmapFT = case functorFT :: HasTrie t :- Functor (f t) of
    Sub Dict -> fmap

instance (FunctorFT f, HasTrie t) => Functor (ExTrie f t) where
    fmap f (ExTrie tfx ex) = ExTrie (fmapTrie tfx) (fmap f ex)
      where
        fmapTrie = trie . fmap (fmapFT f) . untrie

instance (FunctorFT f, HasTrie t) => Functor (BehaviorT f t) where
    fmap f (BehaviorT fx tbx) = BehaviorT (fmapFT f fx) (fmap f tbx)

instance (FunctorFT f, HasTrie t) => Functor (EventResultT f t) where
    fmap f (EventResultT fxs e) = EventResultT (fmapOcc f fxs) (fmap f e)
      where
        fmapOcc = fmap . fmapFT

deriving instance (FunctorFT f, HasTrie t) => Functor (EventT f t)
deriving instance                             Functor (Snd f)

instance FunctorFT f => FunctorFT (BehaviorT f)    where functorFT = Sub Dict
instance FunctorFT f => FunctorFT (EventResultT f) where functorFT = Sub Dict
instance FunctorFT f => FunctorFT (EventT f)       where functorFT = Sub Dict
instance                FunctorFT Snd              where functorFT = Sub Dict


-- Same difficulty with Applicative. There is an Applicative instance for
-- Behavior, but not for Event.

class FunctorFT f => ApplicativeFT f where
    applicativeFT :: HasTrie t :- Applicative (f t)

pureFT :: forall f t a. (ApplicativeFT f, HasTrie t)
       => a -> f t a
pureFT = case applicativeFT :: HasTrie t :- Applicative (f t) of
    Sub Dict -> pure

apFT :: forall f t a b. (ApplicativeFT f, HasTrie t)
     => f t (a -> b) -> f t a -> f t b
apFT = case applicativeFT :: HasTrie t :- Applicative (f t) of
    Sub Dict -> (<*>)

instance (ApplicativeFT f, HasTrie t) => Applicative (ExTrie f t) where
    pure x = ExTrie (pure (pureFT x)) (pure x)
    ExTrie tff fEx <*> ExTrie tfx xEx = ExTrie (apFT <$> tff <*> tfx)
                                               (fEx <*> xEx)

instance (ApplicativeFT f, HasTrie t) => Applicative (BehaviorT f t) where
    pure x = BehaviorT (pureFT x) (pure x)
    BehaviorT ff tbf <*> BehaviorT fx tbx = BehaviorT (apFT ff fx)
                                                      (tbf <*> tbx)

instance Applicative (Snd f) where
    pure x = Snd x
    Snd f <*> Snd x = Snd (f x)

instance ApplicativeFT f => ApplicativeFT (BehaviorT f) where applicativeFT = Sub Dict
instance                    ApplicativeFT Snd           where applicativeFT = Sub Dict
