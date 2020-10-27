{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PipesClass (
    MonadPipe(..),
    MonadProducer(..),
    MonadConsumer(..)
) where

import Pipes hiding (await, yield)
import qualified Pipes
import Control.Monad.State

class (Monad m) => MonadProducer a m where
    yield :: a -> m ()

class (Monad m) => MonadConsumer a m where
    await :: m a

class (MonadConsumer a m, MonadProducer b m) => MonadPipe a b m where

instance (Monad m) => MonadProducer b (Proxy a' a () b m) where
    yield = Pipes.yield

instance (Monad m) => MonadConsumer a (Proxy () a b' b m) where
    await = Pipes.await

instance (Monad m) => MonadPipe a b (Proxy () a () b m) where

instance (MonadProducer a m) => MonadProducer a (StateT s m) where
    yield = lift . yield

instance (MonadConsumer a m) => MonadConsumer a (StateT s m) where
    await = lift await

instance (MonadPipe a b m) => MonadPipe a b (StateT s m) where