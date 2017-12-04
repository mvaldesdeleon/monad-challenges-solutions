{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs as bs = concat $ map (pairsWith bs) as
    where pairsWith bs a = map ((,) a) bs
