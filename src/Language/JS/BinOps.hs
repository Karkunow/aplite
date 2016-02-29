{-# LANGUAGE OverloadedStrings #-}
module Language.JS.BinOps where
import Prelude hiding (GT, LT)
import Haste

data BinOp
  = Add    | Sub   | Mul  | Div | Mod
  | And    | Or
  | Eq     | Neq   | LT   | GT  | LTE | GTE
  | Shl    | ShrL  | ShrA
  | BitAnd | BitOr | BitXor
    deriving (Eq)

-- | May the given operator cause integer overflow or non-integral integers?
overflowOp :: BinOp -> Bool
overflowOp op = op `elem` [Add, Sub, Div, Mod]

instance JSType BinOp where
  toJSString Add       = "+"
  toJSString Mul       = "*"
  toJSString Sub       = "-"
  toJSString Div       = "/"
  toJSString Mod       = "%"
  toJSString And       = "&&"
  toJSString Or        = "||"
  toJSString Eq        = "=="
  toJSString Neq       = "!="
  toJSString LT        = "<"
  toJSString GT        = ">"
  toJSString LTE       = "<="
  toJSString GTE       = ">="
  toJSString Shl       = "<<"
  toJSString ShrL      = ">>>"
  toJSString ShrA      = ">>"
  toJSString BitAnd    = "&"
  toJSString BitOr     = "|"
  toJSString BitXor    = "^"

instance Show BinOp where
  show Add       = "+"
  show Mul       = "*"
  show Sub       = "-"
  show Div       = "/"
  show Mod       = "%"
  show And       = "&&"
  show Or        = "||"
  show Eq        = "=="
  show Neq       = "!="
  show LT        = "<"
  show GT        = ">"
  show LTE       = "<="
  show GTE       = ">="
  show Shl       = "<<"
  show ShrL      = ">>>"
  show ShrA      = ">>"
  show BitAnd    = "&"
  show BitOr     = "|"
  show BitXor    = "^"

-- | Returns the precedence of the given operator as an int. Higher number
--   means higher priority.
opPrec :: BinOp -> Int
opPrec Mul       = 100
opPrec Div       = 100
opPrec Mod       = 100
opPrec Add       = 70
opPrec Sub       = 70
opPrec Shl       = 60
opPrec ShrA      = 60
opPrec ShrL      = 60
opPrec LT        = 50
opPrec GT        = 50
opPrec LTE       = 50
opPrec GTE       = 50
opPrec Eq        = 30
opPrec Neq       = 30
opPrec BitAnd    = 25
opPrec BitXor    = 24
opPrec BitOr     = 23
opPrec And       = 20
opPrec Or        = 10

-- | Is the given operator associative?
opIsAssoc :: BinOp -> Bool
opIsAssoc Mul    = True
opIsAssoc Add    = True
opIsAssoc BitAnd = True
opIsAssoc BitOr  = True
opIsAssoc BitXor = True
opIsAssoc And    = True
opIsAssoc Or     = True
opIsAssoc _      = False
