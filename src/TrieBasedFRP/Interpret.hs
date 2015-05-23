{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module TrieBasedFRP.Interpret where

import Data.MemoTrie

import TrieBasedFRP.Combinators
import TrieBasedFRP.Types


-- Since our tries are pure values, our 'interpret' function can be pure as
-- well. In reactive-banana, the corresponding function runs inside IO.

-- |
-- >>> interpret id [[1,2,3],[4,5,6]]
-- [[1,2,3],[4,5,6]]
-- 
-- >>> interpret (fmap (+1)) [[1,2,3],[4,5,6]]
-- [[2,3,4],[5,6,7]]
interpret :: forall a b. HasTrie a
          => (forall t. HasTrie t => Event t a -> Event t b)
          -> [[a]]
          -> [[b]]
interpret mkNetwork = takeUntil outputE
  where
    outputE :: Event [a] b
    outputE = mkNetwork (spill (externalEvent Just))
    
    takeUntil :: Event [a] b -> [[a]] -> [[b]]
    takeUntil _ [] = []
    takeUntil e (xs:xss) = ys : takeUntil e' xss
      where
        (ys, e') = runEvent e xs

-- |
-- >>> interpretB (stepper 0) [[1,2,3],[4,5,6]]
-- [0,3,6]
-- 
-- >>> interpretB (fmap (+1) . stepper 0) [[1,2,3],[4,5,6]]
-- [1,4,7]
interpretB :: forall a b. HasTrie a
           => (forall t. HasTrie t => Event t a -> Behavior t b)
           -> [[a]]
           -> [b]
interpretB mkNetwork = takeUntil outputE
  where
    outputE :: Behavior [a] b
    outputE = mkNetwork (spill (externalEvent Just))
    
    takeUntil :: Behavior [a] b -> [[a]] -> [b]
    takeUntil b [] = [currentValue b]
    takeUntil b (xs:xss) = currentValue b : takeUntil b' xss
      where
        b' = runBehavior b xs
