{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

data Card = Card Int String

instance Show Card where
    show (Card r s) = show r ++ s

allPairs :: [a] -> [b] -> [(a,b)]
allPairs as bs = concat $ map (pairsWith bs) as
    where pairsWith bs a = map ((,) a) bs

allCards :: [Int] -> [String] -> [Card]
allCards as bs = map (uncurry Card) $ allPairs as bs
