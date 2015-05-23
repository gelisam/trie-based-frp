{-# LANGUAGE ScopedTypeVariables #-}
module TrieBasedFRP.Combinators where

import Data.Maybe
import Data.MemoTrie

import TrieBasedFRP.Types
import TrieBasedFRP.Weaken

-- $setup
-- >>> import TrieBasedFRP.Interpret


-- |
-- >>> interpret (\_ -> never) [[1,2,3],[4,5,6]]
-- [[],[]]
never :: HasTrie t => EventT f t a
never = mempty

-- |
-- >>> interpret (\e -> e `union` e) [[1,2,3],[4,5,6]]
-- [[1,2,3,1,2,3],[4,5,6,4,5,6]]
union :: HasTrie t => EventT f t a -> EventT f t a -> EventT f t a
union = mappend


-- |
-- >>> interpret (filterE even) [[1,2,3],[4,5,6]]
-- [[2],[4,6]]
filterE :: forall t a. HasTrie t
        => (a -> Bool)
        -> Event t a
        -> Event t a
filterE p e = mkEvent go (filterE p (weaken e))
  where
    go :: t -> ([a], Event t a)
    go t = (filter p xs, filterE p e')
      where
        (xs, e') = runEvent e t

-- |
-- >>> let input = [[Right 1,Left 2,Right 3],[Left 4 :: Either Int Int]]
-- >>> let fromRight = either (const Nothing) Just
-- >>> interpret (filterJust . fmap fromRight) input
-- [[1,3],[]]
filterJust :: HasTrie t => Event t (Maybe a) -> Event t a
filterJust = fmap fromJust . filterE isJust


-- |
-- >>> interpret (accumE 0 . fmap (+)) [[1,2,3],[4,5,6]]
-- [[1,3,6],[10,15,21]]
accumE :: forall t a. HasTrie t
       => a
       -> Event t (a -> a)
       -> Event t a
accumE x e = mkEvent go (accumE x (weaken e))
  where
    go :: t -> ([a], Event t a)
    go t = x' `seq` (xs', accumE x' e')
      where
        (fs, e') = runEvent e t
        xs = scanl (flip ($)) x fs
        x' = last xs
        xs' = tail xs  -- skip the initial unmodified x

-- |
-- >>> interpretB (stepper 0) [[1,2,3],[4,5,6]]
-- [0,3,6]
stepper :: forall t a. HasTrie t
        => a
        -> Event t a
        -> Behavior t a
stepper x e = mkBehavior x go (stepper x (weaken e))
  where
    go :: t -> Behavior t a
    go t = stepper x' e'
      where
        (xs, e') = runEvent e t
        x' = last (x:xs)


infixl 4 <@>
infixl 4 <@

-- |
-- >>> interpret (\e -> (*) <$> stepper 1 e <@> e) [[1,2,3],[4,5,6]]
-- [[1,2,3],[12,15,18]]
(<@>) :: forall t a b. HasTrie t
      => Behavior t (a -> b)
      -> Event t a
      -> Event t b
b <@> e = mkEvent go (weaken b <@> weaken e)
  where
    go :: t -> ([b], Event t b)
    go t = (fmap f xs, b' <@> e')
      where
        (xs, e') = runEvent e t
        b' = runBehavior b t
        f = currentValue b

-- |
-- >>> interpret (\e -> stepper 0 e <@ e) [[1,2,3],[4,5,6]]
-- [[0,0,0],[3,3,3]]
(<@) :: HasTrie t => Behavior t b -> Event t a -> Event t b
(<@) = (<@>) . fmap const


-- |
-- >>> interpret collect [[1,2,3],[]]
-- [[[1,2,3]],[]]
collect :: forall t a. HasTrie t
        => Event t a
        -> Event t [a]
collect e = mkEvent go (collect (weaken e))
  where
    go :: t -> ([[a]], Event t [a])
    go t = (xss, collect e')
      where
        (xs, e') = runEvent e t
        xss = if null xs then [] else [xs]

-- |
-- >>> interpret spill [[[1,2,3]],[[4],[5,6]]]
-- [[1,2,3],[4,5,6]]
spill :: forall t a. HasTrie t
      => Event t [a]
      -> Event t a
spill e = mkEvent go (spill (weaken e))
  where
    go :: t -> ([a], Event t a)
    go t = (concat xss, spill e')
      where
        (xss, e') = runEvent e t
