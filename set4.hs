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

mapM :: Monad m => (a -> b) -> m a -> m b
mapM f ma = ma `bind` \a -> return $ f a

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = ma `bind` \a -> mb `bind` \b -> return $ f a b

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) = ma `bind` \a -> sequence mas `bind` \as -> return (a:as)

join :: Monad m => m (m a) -> m a
join ma = ma `bind` id

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma = mf `bind` \f -> ma `bind` \a -> return $ f a

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc = ma `bind` \a -> mb `bind` \b -> mc `bind` \c -> return $ f a b c

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind
