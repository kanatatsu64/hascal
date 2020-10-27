# calc

## syntax

```bnf
Cmd := Expr0 ; | Expr0 ; Cmd | Expr0
Expr0 := <Label> = Expr1 | Expr1
Expr1 := Expr2 + Expr1 | Expr2 - Expr1 | Expr2
Expr2 := Expr3 * Expr2 | Expr3 / Expr2 | Expr3
Expr3 := (Cmd) | <Label> | <Value>

<Label> :: String
<Value> :: Float
```

```shell:example
> var = 1 + (2 + 3) * 4 ; var + 1.2
22.2
```