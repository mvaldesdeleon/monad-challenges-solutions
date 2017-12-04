{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing
             | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

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
queryGreek d k = lookupMay k d `bindMaybe`
                 \xs ->
                    tailMay xs `bindMaybe`
                    maximumMay `bindMaybe`
                    \max ->
                        headMay xs `bindMaybe`
                        (divMay (fromIntegral max) . fromIntegral)

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe m f = case m of
                    Nothing -> Nothing
                    Just a -> f a

pureMaybe :: a -> Maybe a
pureMaybe = Just

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries m a b = liftMaybe2 (+) (lookupMay a m) (lookupMay b m)

liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftMaybe2 f ma mb = ma `bindMaybe`
                     \a ->
                        mb `bindMaybe`
                        \b ->
                            pureMaybe (f a b)

tailProd :: Num a => [a] -> Maybe a
tailProd = tailFold (*) 1

tailSum :: Num a => [a] -> Maybe a
tailSum = tailFold (+) 0

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f ma = ma `bindMaybe`
                \a ->
                    pureMaybe $ f a

tailFold :: (b -> a -> b) -> b -> [a] -> Maybe b
tailFold f z xs = foldl f z `mapMaybe` tailMay xs

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax xs = maximumMay `mapMaybe` tailMay xs

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin xs = minimumMay `mapMaybe` tailMay xs

joinMaybe :: Maybe (Maybe a) -> Maybe a
joinMaybe mma = mma `bindMaybe` id
