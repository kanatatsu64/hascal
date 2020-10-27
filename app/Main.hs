module Main where

{-
    Cmd := <Label> = Expr1 | Expr1 ; Cmd
    Expr1 := Expr2 + Expr1 | Expr2 - Expr1 | Expr2
    Expr2 := Expr3 * Expr2 | Expr3 / Expr2 | Expr3
    Expr3 := (Cmd) | <Label> | Value
    Value := <Float>

    > var = 1 + (2 + 3) * 4 ; var + 1.2
    22.2
-}

main :: IO ()
main = return ()