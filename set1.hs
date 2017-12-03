{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands :: [Integer]
fiveRands = getRands (mkSeed 1) 5

getRands :: Seed -> Integer -> [Integer]
getRands s n
    | n <= 0 = []
    | otherwise = let (r, ns) = rand s in r : getRands ns (n - 1)
