{-# LANGUAGE ScopedTypeVariables #-}
module TrieBasedFRP.Combinators where

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
