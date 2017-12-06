{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude
import Set4

data Card = Card Int String

instance Show Card where
    show (Card r s) = show r ++ s

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = liftM2 (,)

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 Card

liftList3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
liftList3 f as bs cs = f `mapM` as `ap` bs `ap` cs

apList :: [a -> b] -> [a] -> [b]
apList = ap
