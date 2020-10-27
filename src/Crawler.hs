{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Crawler (
    MonadCrawl(..),
    CrawlState(..)
) where

import Control.Monad.State

import PipesClass ( MonadConsumer(..) )

{-
    read >> read = read
    skip >> read = next
    read >> next = next

    flush >> flush -> []
    reset >> flush -> []
-}
class Monad m => MonadCrawl a m | m -> a where
    next :: m a
    read :: m a
    store :: m ()
    flush :: m [a]
    skip :: m ()
    reset :: m ()

    skip = void next
    reset = void flush

data CrawlState a = CrawlState { buffer :: [a], storage :: [a] }

liftBuffer :: Monad m => StateT [a] m b -> StateT (CrawlState a) m b
liftBuffer s = StateT $ \t -> do
                  (a, ls) <- runStateT s (buffer t)
                  return (a, t { buffer = ls })

liftStorage :: Monad m => StateT [a] m b -> StateT (CrawlState a) m b
liftStorage s = StateT $ \t -> do
                  (a, ls) <- runStateT s (storage t)
                  return (a, t { storage = ls })

instance (MonadConsumer a m) => MonadCrawl a (StateT (CrawlState a) m) where
    next = do
        ls <- liftBuffer get
        case ls of
            (x:xs) -> do
                liftBuffer $ put xs
                return x
            [] -> do
                x <- await
                return x

    read = do
        ls <- liftBuffer get
        case ls of
            (x:_) -> return x
            [] -> do
                x <- await
                liftBuffer $ put [x]
                return x

    store = do
        c <- next
        liftStorage $ modify (c:)

    flush = do
        ls <- liftStorage $ get
        liftStorage $ put []
        return ls