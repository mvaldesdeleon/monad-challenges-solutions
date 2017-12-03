{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = getRands rand (mkSeed 1) 5

getRands :: Gen a -> Seed -> Integer -> [a]
getRands gen s n
    | n <= 0 = []
    | otherwise = let (r, ns) = gen s in r : getRands gen ns (n - 1)

randLetter :: Gen Char
randLetter s = let (r, ns) = rand s in (toLetter r, ns)

randString3 :: String
randString3 = getRands randLetter (mkSeed 1) 3
