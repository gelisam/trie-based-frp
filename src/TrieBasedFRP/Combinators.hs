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
