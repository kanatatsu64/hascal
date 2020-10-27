module Types (
    ValueType,
    TargetType,
    eof,
    add,
    sub,
    mul,
    div
) where

import Prelude hiding ( div )

type ValueType = Float
type TargetType = ValueType

eof :: Char
eof = '\0'

add :: ValueType -> ValueType -> ValueType
add = (+)

sub :: ValueType -> ValueType -> ValueType
sub = (-)

mul :: ValueType -> ValueType -> ValueType
mul = (*)

div :: ValueType -> ValueType -> ValueType
div = (/)