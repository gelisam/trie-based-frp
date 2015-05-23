module TrieBasedFRP.Combinators where

import Data.MemoTrie

import TrieBasedFRP.Types


never :: HasTrie t => EventT f t a
never = mempty

union :: HasTrie t => EventT f t a -> EventT f t a -> EventT f t a
union = mappend
