{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval (
    Table(..),
    eval
) where

import Control.Monad.Except
import Control.Monad.State
import Pipes ( Consumer, await )
import Data.Map (Map)
import qualified Data.Map as Map

import Types (ValueType, TargetType)
import PipesClass ( MonadConsumer )
import AST

type Table = Map String ValueType
newtype Eval m a = Eval (StateT Table (Consumer AST m) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState Table,
        MonadError e,
        MonadConsumer AST
    )

eval :: MonadError String m => Table -> Consumer AST m TargetType
eval t = do
    a <- await
    let Eval s = evaluator a
    evalStateT s t

evaluator :: MonadError String m => AST -> Eval m ValueType
evaluator n@(Node _ _ _) = nodeEvaluator n
evaluator t@(Term _) = termEvaluator t

nodeEvaluator :: MonadError String m => AST -> Eval m ValueType
nodeEvaluator (Node a l r) = do
    case a of
        Operator f -> do
            lv <- evaluator l
            rv <- evaluator r
            return $ f lv rv
        Assigner -> do
            k <- case l of
                Term (Label l) -> return l
                _ -> throwError $ "expected Label but given: " <> show l
            v <- evaluator r
            modify (Map.insert k v)
            return v
        Sequence -> do
            evaluator l
            v <- evaluator r
            return v
nodeEvaluator (Term _) = throwError $ "expected Node but given Term"

termEvaluator :: MonadError String m => AST -> Eval m ValueType
termEvaluator (Term a) = do
    case a of
        Label l -> do
            t <- get
            case Map.lookup l t of
                Just v -> return v
                Nothing -> throwError $ "variable not found: " <> l
        Value v -> return v
termEvaluator (Node _ _ _) = throwError $ "expected Term but given Node"