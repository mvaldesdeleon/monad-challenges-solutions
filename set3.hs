{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

data Card = Card Int String

instance Show Card where
    show (Card r s) = show r ++ s

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = liftList2 (,)

allCards :: [Int] -> [String] -> [Card]
allCards = liftList2 Card

liftList2 :: (a -> b -> c) -> [a] -> [b] -> [c]
liftList2 f as bs = f `map` as `apList` bs

liftList3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
liftList3 f as bs cs = f `map` as `apList` bs `apList` cs

apList :: [a -> b] -> [a] -> [b]
apList fs as = concat $ map (\a -> map ($ a) fs) as
