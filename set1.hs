{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude
import Set4

-- type Gen a = Seed -> (a, Seed)
randInt :: Gen Integer
randInt = Gen rand

fiveRands :: [Integer]
fiveRands = getRands randInt (mkSeed 1) 5

getRands :: Gen a -> Seed -> Integer -> [a]
getRands gen s n
    | n <= 0 = []
    | otherwise = let (r, ns) = runGen gen s in r : getRands gen ns (n - 1)

randLetter :: Gen Char
randLetter = mapM toLetter randInt

randString3 :: String
randString3 = getRands randLetter (mkSeed 1) 3

randEven :: Gen Integer -- the output of rand * 2
randEven = mapM (*2) randInt

randOdd :: Gen Integer -- the output of randEven + 1
randOdd = mapM (+1) randEven

randTen :: Gen Integer -- the output of rand * 10
randTen = mapM (*10) randInt

randPair :: Gen (Char, Integer)
randPair = generalPair randLetter randInt

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair ga gb = liftM2 (,) ga gb

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence
