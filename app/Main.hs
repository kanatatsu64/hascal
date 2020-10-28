{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

{-
    Cmd := Expr0 ; | Expr0 ; Cmd | Expr0
    Expr0 := <Label> = Expr1 | Expr1
    Expr1 := Expr2 + Expr1 | Expr2 - Expr1 | Expr2
    Expr2 := Expr3 * Expr2 | Expr3 / Expr2 | Expr3
    Expr3 := (Cmd) | <Label> | <Value>

    <Label> :: String (a-z A-Z)
    <Value> :: Int

    > var = 10 + (20 + 30) * 4 ; var + 12
    222
-}

import Control.Monad.Except
import qualified Data.Map as Map
import Pipes ( (>->), runEffect )

import Source ( fromStdIO )
import Token ( tokenize )
import AST ( build )
import Eval ( eval )

newtype MainType a = MainType (ExceptT String IO a)
    deriving (
        Functor,
        Applicative,
        Monad,
        MonadIO,
        MonadError String
    )

table = Map.empty

main :: IO ()
main = do
    r <- runExceptT $ do
        let p = fromStdIO >-> tokenize >-> build >-> eval table
        runEffect p
    case r of
        Right v -> print v
        Left e -> print e