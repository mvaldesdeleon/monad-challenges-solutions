{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2

class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a

instance Monad [] where
    bind as f = concat $ map f as
    return a = [a]

instance Monad Maybe where
    bind Nothing  _ = Nothing
    bind (Just a) f = f a
    return a = Just a

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen = ((.).(.)) fst runGen

instance Monad Gen where
    bind (Gen ga) f = Gen $ \s -> let (a, ns) = ga s
                                      Gen gb = f a
                                      in gb ns
    return a = Gen $ \s -> (a, s)

-- From Set 1
-- mapRand :: (a -> b) -> Gen a -> Gen b
-- liftRand2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
-- pureRand :: a -> Gen a
-- bindRand :: Gen a -> (a -> Gen b) -> Gen b
--
-- repRandom :: [Gen a] -> Gen [a]

-- From Set 2
-- mapMaybe :: (a -> b) -> Maybe a -> Maybe b
-- liftMaybe2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- pureMaybe :: a -> Maybe a
-- bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
--
-- joinMaybe :: Maybe (Maybe a) -> Maybe a

-- Generics
-- mapM :: (a -> b) -> m a -> m b
-- liftM2 :: (a -> b -> c) m a -> m b -> m c
-- pure :: a -> m a
-- bind :: m a -> (a -> m b) -> m b
--
-- sequence :: [m a] -> m [a]
-- join :: m (m a) -> m a
