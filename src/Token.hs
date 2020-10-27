{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Token (
  Token,
  OpType,
  PrType,
  tokenize
) where

import Pipes (Pipe)
import Control.Monad.State
import Control.Monad.Except
import Data.Char (isLetter, isDigit)
import Text.Read (readMaybe)

import PipesClass

data OpType = Add | Sub | Mul | Div
data PrType = Open | Close
data Token = Label String | Operator OpType | Assigner | Separator | Parentheses PrType | Value Float

newtype Tokenizer m a = Tokenizer (StateT [Char] (Pipe Char Token m) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState [Char],
        MonadError e,
        MonadProducer Token,
        MonadConsumer Char
    )

tokenize :: MonadError String m => Pipe Char Token m ()
tokenize = let Tokenizer s = tokenizer
           in evalStateT s []

tokenizer :: MonadError String m => Tokenizer m ()
tokenizer = do
    c <- await
    modify (c:)
    if isWhiteSpace c
      then skipper
      else return ()
    token <- case () of
              _ | isDigit c -> valueTokenizer
                | isLetter c -> labelTokenizer
                | isOperator c -> operatorTokenizer
                | isParentheses c -> parenthesesTokenizer
                | isAssigner c -> assignerTokenizer
                | isSeparator c -> separatorTokenizer
                | otherwise -> throwError $ "invalid charactor: " <> show c
    yield token

skipper :: MonadError String m => Tokenizer m ()
skipper = do
    c <- loop
    str <- get
    if validate str
      then return ()
      else throwError $ "invalid white spaces: " <> str
    put [c]
  where loop :: Monad m => Tokenizer m Char
        loop = do
          c <- await
          if isWhiteSpace c
            then do
              modify (c:)
              loop
            else return c
        validate :: [Char] -> Bool
        validate cs = foldl (&&) True $ isWhiteSpace <$> cs

valueTokenizer :: MonadError String m => Tokenizer m Token
valueTokenizer = do
    c <- loop
    str <- get
    let val :: Maybe Float
        val = readMaybe str
    token <- case val of
      Just x -> return $ Value x
      Nothing -> throwError $ "invalid value: " <> str
    put [c]
    return token
  where loop :: Monad m => Tokenizer m Char
        loop = do
          c <- await
          if isDigit c
            then do
              modify (c:)
              loop
            else return c

labelTokenizer :: MonadError String m => Tokenizer m Token
labelTokenizer = do
    c <- loop
    str <- get
    put [c]
    if validate str
      then return $ Label str
      else throwError $ "invalid label: " <> str
  where loop :: Monad m => Tokenizer m Char
        loop = do
          c <- await
          if isLetter c
            then do
              modify (c:)
              loop
            else return c
        validate :: [Char] -> Bool
        validate cs = foldl (&&) True $ isLetter <$> cs

operatorTokenizer :: MonadError String m => Tokenizer m Token
operatorTokenizer = do
    str <- get
    token <- if validate str
      then case toOpType str of
        Just x -> return $ Operator x
        Nothing -> throwError $ "invalid operator: " <> str
      else throwError $ "invalid operator: " <> str
    put []
    return token
  where validate :: [Char] -> Bool
        validate (c:[]) = isOperator c
        validate _ = False
        toOpType :: [Char] -> Maybe OpType
        toOpType "+" = return Add
        toOpType "-" = return Sub
        toOpType "*" = return Mul
        toOpType "/" = return Div
        toOpType _ = Nothing

assignerTokenizer :: MonadError String m => Tokenizer m Token
assignerTokenizer = do
    str <- get
    token <- if validate str
      then return Assigner
      else throwError $ "invalid assigner: " <> str
    put []
    return token
  where validate :: [Char] -> Bool
        validate ('=':[]) = True
        validate _ = False

separatorTokenizer :: MonadError String m => Tokenizer m Token
separatorTokenizer = do
    str <- get
    token <- if validate str
      then return Separator
      else throwError $ "invalid separator: " <> str
    put []
    return token
  where validate :: [Char] -> Bool
        validate (';':[]) = True
        validate _ = False

parenthesesTokenizer :: MonadError String m => Tokenizer m Token
parenthesesTokenizer = do
    str <- get
    token <- if validate str
      then case toPrType str of
        Just x -> return $ Parentheses x
        Nothing -> throwError $ "invalid parentheses: " <> str
      else throwError $ "invalid parentheses: " <> str
    put []
    return token
  where validate :: [Char] -> Bool
        validate (c:[]) = case c of
          '(' -> True
          ')' -> True
          _ -> False
        validate _ = False
        toPrType :: [Char] -> Maybe PrType
        toPrType "(" = return Open
        toPrType ")" = return Close
        toPrType _ = Nothing

isOperator :: Char -> Bool
isOperator = \case
    '+' -> True
    '-' -> True
    '*' -> True
    '/' -> True
    _ -> False

isParentheses :: Char -> Bool
isParentheses = \case
    '(' -> True
    ')' -> True
    _ -> False

isAssigner :: Char -> Bool
isAssigner = \case
    '=' -> True
    _ -> False

isSeparator :: Char -> Bool
isSeparator = \case
    ';' -> True
    _ -> False

isWhiteSpace :: Char -> Bool
isWhiteSpace = \case
    ' ' -> True
    _ -> False