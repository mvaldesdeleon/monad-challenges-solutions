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

mapRand :: (a -> b) -> Gen a -> Gen b
mapRand f ra s = let (a, ns) = ra s in (f a, ns)

randLetter :: Gen Char
randLetter = mapRand toLetter rand

randString3 :: String
randString3 = getRands randLetter (mkSeed 1) 3

randEven :: Gen Integer -- the output of rand * 2
randEven = mapRand (*2) rand

randOdd :: Gen Integer -- the output of randEven + 1
randOdd = mapRand (+1) randEven

randTen :: Gen Integer -- the output of rand * 10
randTen = mapRand (*10) rand
