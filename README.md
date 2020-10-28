# calc

## syntax

```bnf
Cmd := Expr0 ; | Expr0 ; Cmd | Expr0
Expr0 := <Label> = Expr1 | Expr1
Expr1 := Expr2 + Expr1 | Expr2 - Expr1 | Expr2
Expr2 := Expr3 * Expr2 | Expr3 / Expr2 | Expr3
Expr3 := (Cmd) | <Label> | <Value>

<Label> :: String (a-z A-Z)
<Value> :: Int
```

```shell:example
> var = 10 + (20 + 30) * 4 ; var + 12
222
```
