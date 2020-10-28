{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module AST (
    AST(..),
    TermAttr(..),
    NodeAttr(..),
    build
) where

import Prelude hiding ( read )
import Pipes ( Pipe, yield )
import Control.Monad.State
import Control.Monad.Except

import Types ( ValueType, TargetType )
import qualified Types
import Crawler ( CrawlState(..), MonadCrawl(..) )
import PipesClass ( MonadConsumer )
import Token ( Token )
import qualified Token ( Token(..), OpType(..), PrType(..) )

data TermAttr = Label String | Value ValueType deriving Show
data NodeAttr = Operator (ValueType -> ValueType -> ValueType) | Assigner | Sequence
data AST = Node NodeAttr AST AST | Term TermAttr deriving Show

instance Show NodeAttr where
    show (Operator _) = "Operator"
    show Assigner = "Assigner"
    show Sequence = "Sequence"

newtype Builder m a = Builder (StateT (CrawlState Token) (Pipe Token AST m) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState (CrawlState Token),
        MonadError e,
        MonadConsumer Token,
        MonadCrawl Token
    )

build :: MonadError String m => Pipe Token AST m TargetType
build = do
    let Builder s = builder
    a <- evalStateT s $ CrawlState [] []
    yield a
    throwError $ "unexpected end of stream @ AST"

notExpectedMsg :: Token -> Token -> String
notExpectedMsg e a = "expected " <> show e <> " but given " <> show a

builder :: MonadError String m => Builder m AST
builder = do
    a <- cmdBuilder
    x <- read
    case x of
        Token.EOF -> do
            return a
        _ -> throwError $ notExpectedMsg Token.EOF x

cmdBuilder :: MonadError String m => Builder m AST
cmdBuilder = do
    lh <- expr0Builder
    x <- read
    case x of
        Token.Separator -> do
            skip
            y <- read
            case y of
                Token.EOF -> do
                    return lh
                _ -> do
                    rh <- cmdBuilder
                    return $ Node Sequence lh rh
        _ -> return lh

expr0Builder :: MonadError String m => Builder m AST
expr0Builder = do
    x <- read
    case x of
        Token.Label l -> do
            store
            y <- read
            case y of
                Token.Assigner -> do
                    flush >> skip
                    let lh = Term $ Label l
                    rh <- expr1Builder
                    return $ Node Assigner lh rh
                _ -> do
                    restore
                    expr1Builder
        _ -> expr1Builder

expr1Builder :: MonadError String m => Builder m AST
expr1Builder = do
    lh <- expr2Builder
    x <- read
    case x of
        Token.Operator Token.Add -> do
            skip
            rh <- expr1Builder
            return $ Node (Operator Types.add) lh rh
        Token.Operator Token.Sub -> do
            skip
            rh <- expr1Builder
            return $ Node (Operator Types.sub) lh rh
        _ -> return lh

expr2Builder :: MonadError String m => Builder m AST
expr2Builder = do
    lh <- expr3Builder
    x <- read
    case x of
        Token.Operator Token.Mul -> do
            skip
            rh <- expr2Builder
            return $ Node (Operator Types.mul) lh rh
        Token.Operator Token.Div -> do
            skip
            rh <- expr2Builder
            return $ Node (Operator Types.div) lh rh
        _ -> return lh

expr3Builder :: MonadError String m => Builder m AST
expr3Builder = do
    x <- read
    case x of
        Token.Parentheses Token.Open -> do
            skip
            mh <- cmdBuilder
            y <- read
            case y of
                Token.Parentheses Token.Close -> do
                    skip
                    return mh
                _ -> throwError $ notExpectedMsg (Token.Parentheses Token.Close) y
        Token.Label l -> do
            skip
            return $ Term (Label l)
        Token.Value v -> do
            skip
            return $ Term (Value v)
        _ -> throwError $ "unexpected token: " <> show x