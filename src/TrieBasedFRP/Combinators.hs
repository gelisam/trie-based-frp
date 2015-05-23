module TrieBasedFRP.Combinators where

import Data.MemoTrie

import TrieBasedFRP.Types

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
