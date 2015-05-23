{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module TrieBasedFRP.Interpret where

import Control.Arrow
import Data.MemoTrie

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
    outputE :: Event a b
    outputE = mkNetwork (externalEvent Just)
    
    takeUntil :: Event a b -> [[a]] -> [[b]]
    takeUntil _ [] = []
    takeUntil e (xs:xss) = ys : takeUntil e' xss
      where
        (ys, e') = runEvents e xs
    
    runEvents :: Event a b -> [a] -> ([b], Event a b)
    runEvents e [] = ([], e)
    runEvents e (x:xs) = first (ys ++) (runEvents e' xs)
      where
        (ys, e') = runEvent e x
