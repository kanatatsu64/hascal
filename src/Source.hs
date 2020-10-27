{-# LANGUAGE FlexibleContexts #-}

module Source (
    fromString,
    fromStdIO
) where

import Control.Monad.Except
import Pipes (Producer, yield)
import qualified Pipes

import Types (TargetType)
import qualified Types

fromString :: MonadError String m => String -> Producer Char m TargetType
fromString s = do
    Pipes.each s
    yield Types.eof
    throwError "unexpected end of stream"

fromStdIO :: (MonadError String m, MonadIO m) => Producer Char m TargetType
fromStdIO = do
    s <- liftIO $ getLine
    Pipes.each s
    yield Types.eof
    throwError "unexpected end of stream"