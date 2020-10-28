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

> var = 10 + (20 + 30) * 4 ; var + 12
222
```

```shell:example
> var = 1 + (2 + 3) * 4 ; var + 1.2
22.2
```