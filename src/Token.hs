{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Token (
  Token(..),
  OpType(..),
  PrType(..),
  tokenize
) where

import Pipes (Pipe)
import Control.Monad.State
import Control.Monad.Except
import Data.Char (isLetter, isDigit)
import Text.Read (readMaybe)

import PipesClass

data OpType = Add | Sub | Mul | Div deriving Show

data PrType = Open | Close deriving Show

data Token = Label String | Operator OpType
           | Assigner | Separator | Parentheses PrType
           | Value Float | EOF
    deriving Show

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
    case () of
      _ | isDigit c -> valueTokenizer
        | isLetter c -> labelTokenizer
        | isOperator c -> operatorTokenizer
        | isParentheses c -> parenthesesTokenizer
        | isAssigner c -> assignerTokenizer
        | isSeparator c -> separatorTokenizer
        | isEOF c -> eofTokenizer
        | otherwise -> throwError $ "invalid charactor: " <> show c

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

valueTokenizer :: MonadError String m => Tokenizer m ()
valueTokenizer = do
    c <- loop
    str <- get
    let val :: Maybe Float
        val = readMaybe str
    case val of
      Just x -> yield $ Value x
      Nothing -> throwError $ "invalid value: " <> str
    put [c]
  where loop :: Monad m => Tokenizer m Char
        loop = do
          c <- await
          if isDigit c
            then do
              modify (c:)
              loop
            else return c

labelTokenizer :: MonadError String m => Tokenizer m ()
labelTokenizer = do
    c <- loop
    str <- get
    put [c]
    if validate str
      then yield $ Label str
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

operatorTokenizer :: MonadError String m => Tokenizer m ()
operatorTokenizer = do
    str <- get
    if validate str
      then case toOpType str of
        Just x -> yield $ Operator x
        Nothing -> throwError $ "invalid operator: " <> str
      else throwError $ "invalid operator: " <> str
    put []
  where validate :: [Char] -> Bool
        validate (c:[]) = isOperator c
        validate _ = False
        toOpType :: [Char] -> Maybe OpType
        toOpType "+" = return Add
        toOpType "-" = return Sub
        toOpType "*" = return Mul
        toOpType "/" = return Div
        toOpType _ = Nothing

assignerTokenizer :: MonadError String m => Tokenizer m ()
assignerTokenizer = do
    str <- get
    if validate str
      then yield Assigner
      else throwError $ "invalid assigner: " <> str
    put []
  where validate :: [Char] -> Bool
        validate ('=':[]) = True
        validate _ = False

separatorTokenizer :: MonadError String m => Tokenizer m ()
separatorTokenizer = do
    str <- get
    if validate str
      then yield Separator
      else throwError $ "invalid separator: " <> str
    put []
  where validate :: [Char] -> Bool
        validate (';':[]) = True
        validate _ = False

parenthesesTokenizer :: MonadError String m => Tokenizer m ()
parenthesesTokenizer = do
    str <- get
    if validate str
      then case toPrType str of
        Just x -> yield $ Parentheses x
        Nothing -> throwError $ "invalid parentheses: " <> str
      else throwError $ "invalid parentheses: " <> str
    put []
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

eofTokenizer :: MonadError String m => Tokenizer m ()
eofTokenizer = do
    str <- get
    if validate str
      then yield EOF
      else throwError $ "invalid end of file: " <> str
    put []
  where validate :: [Char] -> Bool
        validate ('\0':[]) = True
        validate _ = False

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

isEOF :: Char -> Bool
isEOF = \case
    '\0' -> True
    _ -> False