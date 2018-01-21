{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

    fail :: String -> m a
    fail = undefined

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen = ((.).(.)) fst runGen

instance Monad Gen where
    (Gen ga) >>= f = Gen $ \s -> let (a, ns) = ga s
                                     Gen gb = f a
                                     in gb ns
    return a = Gen $ \s -> (a, s)

makeRandom :: Gen Integer
makeRandom = Gen rand

fiveRands :: Gen [Integer]
fiveRands = do r1 <- makeRandom
               r2 <- makeRandom
               r3 <- makeRandom
               r4 <- makeRandom
               r5 <- makeRandom
               return [r1, r2, r3, r4, r5]

randLetter :: Gen Char
randLetter = do r <- makeRandom
                return (toLetter r)

randString3 :: Gen String
randString3 = do r1 <- randLetter
                 r2 <- randLetter
                 r3 <- randLetter
                 return [r1, r2, r3]

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = do a <- ga
                       b <- gb
                       return (a, b)
