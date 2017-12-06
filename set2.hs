{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude
import Set4

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay a [] = Nothing
lookupMay a (p:ps) = if fst p == a then Just $ snd p
                                   else lookupMay a ps

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay a b = if b == 0 then Nothing
                       else Just (a / b)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just $ foldl max x xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just $ foldl min x xs

queryGreek :: GreekData -> String -> Maybe Double
queryGreek d k = lookupMay k d `bind`
                 \xs ->
                    tailMay xs `bind`
                    maximumMay `bind`
                    \max ->
                        headMay xs `bind`
                        (divMay (fromIntegral max) . fromIntegral)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries m a b = liftM2 (+) (lookupMay a m) (lookupMay b m)

tailProd :: Num a => [a] -> Maybe a
tailProd = tailFold (*) 1

tailSum :: Num a => [a] -> Maybe a
tailSum = tailFold (+) 0

tailFold :: (b -> a -> b) -> b -> [a] -> Maybe b
tailFold f z xs = foldl f z `mapM` tailMay xs

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = maximumMay `mapM` tailMay xs

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin xs = minimumMay `mapM` tailMay xs

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe = join
