{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module AST (
    AST(..),
    TermAttr(..),
    NodeAttr(..),
    build
) where

import Pipes (Consumer)
import Control.Monad.State
import Control.Monad.Except

import PipesClass
import Token (Token)
import qualified Token (Token(..), OpType(..), PrType(..))

data TermAttr = Label String | Value Float
data NodeAttr = Operator (Float -> Float -> Float) | Assigner | Sequence
data AST = Node NodeAttr AST AST | Term TermAttr

newtype Builder m a = Builder (StateT [Token] (Consumer Token m) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState [Token],
        MonadError e,
        MonadConsumer Token
    )

build :: MonadError String m => Consumer Token m AST
build = do
    let Builder s = builder
    a <- evalStateT s []
    return a

next :: Monad m => Builder m Token
next = do
    ls <- get
    case ls of
        (x:xs) -> do
            put xs
            return x
        [] -> do
            x <- await
            modify (x:)
            return x

flush :: Monad m => Builder m ()
flush = put []

notExpectedMsg :: Token -> Token -> String
notExpectedMsg e a = "expected " <> show e <> " but given " <> show a

builder :: MonadError String m => Builder m AST
builder = do
    a <- cmdBuilder
    x <- next
    case x of
        Token.EOF -> return a
        _ -> throwError $ notExpectedMsg Token.EOF x

cmdBuilder :: MonadError String m => Builder m AST
cmdBuilder = do
    lh <- expr0Builder
    x <- next
    case x of
        Token.Separator -> do
            flush
            y <- next
            case y of
                Token.EOF -> do
                    flush
                    return lh
                _ -> do
                    rh <- cmdBuilder
                    return $ Node Sequence lh rh

expr0Builder :: MonadError String m => Builder m AST
expr0Builder = do
    x <- next
    case x of
        Token.Label l -> do
            flush
            y <- next
            case y of
                Token.Assigner -> do
                    flush
                    let lh = Term $ Label l
                    rh <- expr1Builder
                    return $ Node Assigner lh rh
                _ -> expr1Builder
        _ -> expr1Builder

expr1Builder :: MonadError String m => Builder m AST
expr1Builder = do
    lh <- expr2Builder
    x <- next
    case x of
        Token.Operator Token.Add -> do
            flush
            rh <- expr1Builder
            return $ Node (Operator (+)) lh rh
        Token.Operator Token.Sub -> do
            flush
            rh <- expr1Builder
            return $ Node (Operator (-)) lh rh
        _ -> return lh

expr2Builder :: MonadError String m => Builder m AST
expr2Builder = do
    lh <- expr3Builder
    x <- next
    case x of
        Token.Operator Token.Mul -> do
            flush
            rh <- expr2Builder
            return $ Node (Operator (*)) lh rh
        Token.Operator Token.Div -> do
            flush
            rh <- expr2Builder
            return $ Node (Operator (/)) lh rh
        _ -> return lh

expr3Builder :: MonadError String m => Builder m AST
expr3Builder = do
    x <- next
    case x of
        Token.Parentheses Token.Open -> do
            flush
            mh <- cmdBuilder
            y <- next
            case y of
                Token.Parentheses Token.Close -> do
                    flush
                    return mh
                _ -> throwError $ notExpectedMsg (Token.Parentheses Token.Close) y
        Token.Label l -> do
            flush
            return $ Term (Label l)
        Token.Value v -> do
            flush
            return $ Term (Value v)