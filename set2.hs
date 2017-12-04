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
queryGreek d k = let mxs = lookupMay k d
                 in case mxs of
                    Nothing -> Nothing
                    Just xs -> let tail = tailMay xs
                               in case tail of
                                    Nothing -> Nothing
                                    Just tl -> let mx = maximumMay tl
                                               in case mx of
                                                    Nothing -> Nothing
                                                    Just max -> let hd = headMay xs
                                                                in case hd of
                                                                    Nothing -> Nothing
                                                                    Just h -> divMay (fromIntegral max) (fromIntegral h)
