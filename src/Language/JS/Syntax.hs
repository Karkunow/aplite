{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
-- | Restricted JavaScript syntax, suitable for generating ASM.js as well as
--   regular JS.
module Language.JS.Syntax
  ( module Language.JS.BinOps
  , ToJSExp (..), ToIdent (..), toTypedExp, sizeof
  , Type (..), Param (..), Typed (..), isPtrTy, typed, param, newArr
  , Func (..), Id (..), Decl (..), VarId, ArrId
  , Exp (..), Stmt (..)
  , StdFun, stdFunName, std_funs, callStdFun
  , std_floor, std_ceiling, std_imul, std_sqrt, std_pow
  , std_cos, std_sin, std_tan, std_atan2
  , jsNull, jsTrue, jsFalse
  , (.>), (.>=), (.<), (.<=), (.==), (./=), (.?)
  , sameTypeAs, mapTyped
  ) where
import Prelude hiding (LT, GT)
import Language.JS.BinOps
import Data.Int
import Data.Word
import Haste hiding (fromString)
import qualified Haste.JSString as S
import Data.String

class ToJSExp a where
  toJSExp :: a -> Exp
  expType :: a -> Type

toTypedExp :: ToJSExp a => a -> Typed Exp
toTypedExp x = Typed (expType x) (toJSExp x)

instance ToJSExp Bool where
  expType _ = Signed
  toJSExp True = Lit 1
  toJSExp _    = Lit 0
instance ToJSExp Int32 where
  expType _ = Signed
  toJSExp = Lit . fromIntegral
instance ToJSExp Word32 where
  expType _ = Unsigned
  toJSExp = Lit . fromIntegral
instance ToJSExp Double where
  expType _ = Double
  toJSExp = Lit
instance ToJSExp Int where
  expType _ = Signed
  toJSExp = Lit . fromIntegral
instance ToJSExp Integer where
  expType _ = Signed
  toJSExp = Lit . fromInteger
instance ToJSExp Word where
  expType _ = Unsigned
  toJSExp = Lit . fromIntegral

-- | Type of a JavaScript expression.
data Type
  = Signed
  | Unsigned
  | Double
  | Arr Type
    deriving (Eq, Show)

sizeof :: Type -> Int
sizeof Signed   = 4
sizeof Unsigned = 4
sizeof Double   = 8
sizeof (Arr _)  = 4

isPtrTy :: Type -> Bool
isPtrTy (Arr _) = True
isPtrTy _       = False

class ToIdent a where
  toIdent :: a -> Id

type VarId = JSString
type ArrId = VarId

-- | A JavaScript identifier.
newtype Id
  = MkId {unId :: VarId}
    deriving Eq

instance Show Id where
  show (MkId x) = S.unpack x

-- | An untyped JavaScript expression with typed subexpressions.
data Exp
  = Id    !Id
  | Op    !BinOp !(Typed Exp) !(Typed Exp)
  | Lit   !Double -- Double can express the full range of Int/Word32
  | Neg   !(Typed Exp)
  | Not   !(Typed Exp)
  | Cast  !Type !(Typed Exp)
  | Cond  !(Typed Exp) !(Typed Exp) !(Typed Exp) -- TODO: how to do this in ASM?
  | Call  !JSString ![Typed Exp]
  | Index !Type !ArrId !(Typed Exp)
    deriving Eq

newArr :: Type -> Typed Exp -> Typed Exp
newArr t sz = Typed (Arr t) $ Call "malloc" [toTypedExp (sizeof t), sz]

-- | Call a standard library function.
callStdFun :: StdFun -> [Typed Exp] -> Exp
callStdFun (StdFun f) = Call f

(.<) :: Typed Exp -> Typed Exp -> Typed Exp
a .< b = typed Signed (Op LT a b)

(.<=) :: Typed Exp -> Typed Exp -> Typed Exp
a .<= b = typed Signed (Op LTE a b)

(.>) :: Typed Exp -> Typed Exp -> Typed Exp
a .> b = typed Signed (Op GT a b)

(.>=) :: Typed Exp -> Typed Exp -> Typed Exp
a .>= b = typed Signed (Op GTE a b)

(.==) :: Typed Exp -> Typed Exp -> Typed Exp
a .== b = typed Signed (Op Eq a b)

(./=) :: Typed Exp -> Typed Exp -> Typed Exp
a ./= b = typed Signed (Op Neq a b)

(.?) :: Typed Exp -> Typed Exp -> Typed Exp -> Typed Exp
(.?) c a b = typed (typeOf a) (Cond c a b)

instance Num (Typed Exp) where
  a@(Typed t _) + b@(Typed t' _) | t == t' = Typed t (Op Add a b)
  a@(Typed t _) - b@(Typed t' _) | t == t' = Typed t (Op Sub a b)
  a@(Typed t _) * b@(Typed t' _) | t == t' = Typed t (Op Mul a b)
  negate      = mapTyped Neg
  abs x       = (x .< 0) .? (sameTypeAs x (Neg x)) $ x
  signum x    = (x .< 0) .? negOne $ (x .> 0) .? one $ zero
    where one    = sameTypeAs x (toJSExp (1 :: Int32))
          zero   = sameTypeAs x (toJSExp (0 :: Int32))
          negOne = sameTypeAs x (toJSExp (-1 :: Int32))
  fromInteger = error $ "Typed Exp needs a type annotation, " ++
                        "which fromInteger can't provide!"

-- | Type annotation for JavaScript AST nodes.
data Typed a = Typed
  { typeOf  :: !Type
  , untyped :: !a
  } deriving Eq

typed :: Type -> a -> Typed a
typed = Typed

-- | b at the same type as that of a.
sameTypeAs :: Typed a -> b -> Typed b
sameTypeAs (Typed t _) x = Typed t x

-- | Maps a type-preserving function over a typed expression.
--   E.g. @not_ = mapTyped Not :: Typed Exp -> Typed Exp@.
mapTyped :: (Typed a -> b) -> Typed a -> Typed b
mapTyped f tx@(Typed t _) = Typed t (f tx)

instance Functor Typed where
  fmap f (Typed t a) = Typed t (f a)

data Param = Param
  { paramType :: !Type
  , paramName :: !Id
  }

-- | Create a function parameter. Unsigned is not a valid function parameter
--   type, so unsigned parameters are turned into equivalent signed ones.
param :: Type -> Id -> Param
param Unsigned x = Param Signed x
param t x        = Param t x

-- | A variable declaration with optional initialization.
data Decl = Decl
  { declType :: !Type
  , declId   :: !Id
  }

-- | A standard library function.
newtype StdFun = StdFun {stdFunName :: JSString}

std_floor, std_ceiling, std_sqrt, std_imul, std_pow,
  std_cos, std_sin, std_tan, std_atan2 :: StdFun
std_floor    = StdFun "floor"
std_ceiling  = StdFun "ceil"
std_sqrt     = StdFun "sqrt"
std_imul     = StdFun "imul"
std_pow      = StdFun "pow"
std_cos      = StdFun "cos"
std_sin      = StdFun "sin"
std_tan      = StdFun "tan"
std_atan2    = StdFun "atan2"

-- | All available standard library functions.
std_funs :: [StdFun]
std_funs =
  [ std_floor, std_ceiling, std_sqrt, std_imul
  , std_pow, std_cos, std_sin, std_tan
  ]

-- | A typed JavaScript statement.
data Stmt
  = !Id :=   !(Typed Exp)
  | DeclStm  !Decl
  | ParamStm !Param
  | ExpStm   !(Typed Exp)
  | Inc      !(Typed Id)
  | Dec      !(Typed Id)
  | Ret      !(Typed Exp)
  | Block    ![Stmt]
  | If       !(Typed Exp) !Stmt !(Maybe Stmt)
  | Forever  !Stmt
  | For      !Stmt !(Typed Exp) !Stmt !Stmt
  | Break
  | Assert   !(Typed Exp) !JSString
  | Write    !ArrId !(Typed Exp) !(Typed Exp)

data Func = Func
  { funParams :: ![Param]
  , funLocals :: ![Decl]
  , funBody   :: ![Stmt]
  }

jsNull :: Typed Exp
jsNull = typed Unsigned (Lit $ -1)

jsTrue :: Typed Exp
jsTrue = typed Signed (Lit 1)

jsFalse :: Typed Exp
jsFalse = typed Signed (Lit 0)
