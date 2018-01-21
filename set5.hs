{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude
import Set2

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

instance Monad Maybe where
    Nothing  >>= _ = Nothing
    (Just a) >>= f = f a
    return a = Just a

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d k = do xs <- lookupMay k d
                    tl <- tailMay xs
                    mx <- maximumMay tl
                    hd <- headMay xs
                    divMay (fromIntegral mx) (fromIntegral hd)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries m a b = do s1 <- lookupMay a m
                       s2 <- lookupMay b m
                       return (s1 + s2)

tailProd :: Num a => [a] -> Maybe a
tailProd xs = do tl <- tailMay xs
                 return $ foldl (*) 1 tl

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do tl <- tailMay xs
                return $ foldl (+) 0 tl

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = do tl <- tailMay xs
                maximumMay tl
