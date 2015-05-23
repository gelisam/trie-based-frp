module TrieBasedFRP.Weaken where

import TrieBasedFRP.Types


-- When constructing a Behavior or an Event out of an input Behavior or Event,
-- we must also construct a version for the extended 't'. To do so efficiently,
-- we should construct it in terms of the part of the input trie which is
-- already extended. Weaken is an accessor for this part.
-- 
-- It's called "Weaken" by analogy with the operation with the same name which
-- converts a term in a context gamma to this same term in an extended context.

class Weaken f where
    weaken :: f t a -> f (Extend t) a

instance Weaken (ExTrie f) where
    weaken = extendedTrie

instance Weaken f => Weaken (BehaviorT f) where
    weaken (BehaviorT fx tbx) = BehaviorT (weaken fx) (weaken tbx)

instance Weaken (EventT f) where
    weaken (EventT trx) = EventT (weaken trx)

instance Weaken Snd where
    weaken (Snd x) = Snd x
