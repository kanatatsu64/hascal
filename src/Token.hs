{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Token (
  Token(..),
  OpType(..),
  PrType(..),
  tokenize
) where

import Prelude hiding (init, read)
import Pipes (Pipe)
import Control.Monad.State
import Control.Monad.Except
import Data.Char (isLetter, isDigit)
import Text.Read (readMaybe)

import Types (ValueType, TargetType)
import qualified Types
import PipesClass
import Crawler

data OpType = Add | Sub | Mul | Div deriving Show

data PrType = Open | Close deriving Show

data Token = Label String | Operator OpType
           | Assigner | Separator | Parentheses PrType
           | Value ValueType | EOF
    deriving Show

newtype Tokenizer m a = Tokenizer (StateT (CrawlState Char) (Pipe Char Token m) a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadState (CrawlState Char),
        MonadError e,
        MonadProducer Token,
        MonadConsumer Char,
        MonadCrawl Char
    )

tokenize :: MonadError String m => Pipe Char Token m TargetType
tokenize = do
    let Tokenizer s = tokenizer
    evalStateT s $ CrawlState [] []
    throwError "unexpected end of stream"

tokenizer :: MonadError String m => Tokenizer m ()
tokenizer = do
    c <- read
    if isWhiteSpace c
      then skipper
      else return ()
    c <- read
    case () of
      _ | isDigit c -> valueTokenizer
        | isLetter c -> labelTokenizer
        | isOperator c -> operatorTokenizer
        | isParentheses c -> parenthesesTokenizer
        | isAssigner c -> assignerTokenizer
        | isSeparator c -> separatorTokenizer
        | isEOF c -> eofTokenizer
        | otherwise -> throwError $ "invalid charactor: " <> show c
    if isEOF c
      then return ()
      else tokenizer

skipper :: MonadError String m => Tokenizer m ()
skipper = do
    loop
    str <- flush
    if validate str
      then return ()
      else throwError $ "invalid white spaces: " <> str
  where loop :: Monad m => Tokenizer m ()
        loop = do
          c <- read
          if isWhiteSpace c
            then do
              store
              loop
            else 
              return ()
        validate :: [Char] -> Bool
        validate cs = foldl (&&) True $ isWhiteSpace <$> cs

valueTokenizer :: MonadError String m => Tokenizer m ()
valueTokenizer = do
    loop
    str <- flush
    let val :: Maybe ValueType
        val = readMaybe str
    case val of
      Just x -> yield $ Value x
      Nothing -> throwError $ "invalid value: " <> str
  where loop :: Monad m => Tokenizer m ()
        loop = do
          c <- read
          if isDigit c
            then do
              store
              loop
            else return ()

labelTokenizer :: MonadError String m => Tokenizer m ()
labelTokenizer = do
    loop
    str <- flush
    if validate str
      then yield $ Label str
      else throwError $ "invalid label: " <> str
  where loop :: Monad m => Tokenizer m ()
        loop = do
          c <- read
          if isLetter c
            then do
              store
              loop
            else return ()
        validate :: [Char] -> Bool
        validate cs = foldl (&&) True $ isLetter <$> cs

operatorTokenizer :: MonadError String m => Tokenizer m ()
operatorTokenizer = do
    c <- read
    if isOperator c
      then
        store
      else
        return ()
    str <- flush
    if validate str
      then case toOpType str of
        Just x -> yield $ Operator x
        Nothing -> throwError $ "invalid operator: " <> str
      else throwError $ "invalid operator: " <> str
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
    c <- read
    if isAssigner c
      then store
      else return ()
    str <- flush
    if validate str
      then yield Assigner
      else throwError $ "invalid assigner: " <> str
  where validate :: [Char] -> Bool
        validate ('=':[]) = True
        validate _ = False

separatorTokenizer :: MonadError String m => Tokenizer m ()
separatorTokenizer = do
    c <- read
    if isSeparator c
      then store
      else return ()
    str <- flush
    if validate str
      then yield Separator
      else throwError $ "invalid separator: " <> str
  where validate :: [Char] -> Bool
        validate (';':[]) = True
        validate _ = False

parenthesesTokenizer :: MonadError String m => Tokenizer m ()
parenthesesTokenizer = do
    c <- read
    if isParentheses c
      then store
      else return ()
    str <- flush
    if validate str
      then case toPrType str of
        Just x -> yield $ Parentheses x
        Nothing -> throwError $ "invalid parentheses: " <> str
      else throwError $ "invalid parentheses: " <> str
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
    c <- read
    if isEOF c
      then store
      else return ()
    str <- flush
    if validate str
      then yield EOF
      else throwError $ "invalid end of file: " <> str
  where validate :: [Char] -> Bool
        validate (x:[]) = isEOF x
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
isEOF c | c == Types.eof = True
        | otherwise = False