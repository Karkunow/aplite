{-# LANGUAGE FlexibleInstances #-}
-- | Restricted JavaScript syntax, suitable for generating ASM.js as well as
--   regular JS.
module Language.JS.Syntax
  ( module Language.JS.BinOps
  , ToJSExp (..)
  , Type (..), isPtrTy
  , Func (..), Id (..), Decl (..) , Param , Typed (..), Exp (..), Stmt (..)
  , jsNull, jsTrue, jsFalse
  , (.>), (.>=), (.<), (.<=), (.==), (./=), (.?)
  , sameTypeAs, mapTyped
  ) where
import Prelude hiding (LT, GT)
import Language.JS.BinOps
import Data.Int
import Data.Word

class ToJSExp a where
  toJSExp :: a -> Exp

instance ToJSExp Bool where
  toJSExp True = Lit 1
  toJSExp _    = Lit 0
instance ToJSExp Int32   where toJSExp = Lit . fromIntegral
instance ToJSExp Word32  where toJSExp = Lit . fromIntegral
instance ToJSExp Double  where toJSExp = Lit
instance ToJSExp Int     where toJSExp = Lit . fromIntegral
instance ToJSExp Integer where toJSExp = Lit . fromInteger
instance ToJSExp Word    where toJSExp = Lit . fromIntegral

{-# WARNING Type "TODO: Unsigned is not a valid argument type!" #-}

-- | Type of a JavaScript expression.
data Type
  = Signed
  | Unsigned
  | Double
  | Arr Type
    deriving Eq

isPtrTy :: Type -> Bool
isPtrTy (Arr _) = True
isPtrTy _       = False

-- | A JavaScript identifier.
newtype Id = MkId {unId :: Integer}
  deriving (Show, Eq, Enum)

-- | An untyped JavaScript expression with typed subexpressions.
data Exp
  = Id   !Id
  | Op   !BinOp !(Typed Exp) !(Typed Exp)
  | Lit  !Double -- Double can express the full range of Int/Word32
  | Neg  !(Typed Exp)
  | Not  !(Typed Exp)
  | Cast !Type !(Typed Exp)
  | Cond !(Typed Exp) !(Typed Exp) !(Typed Exp)
    deriving Eq

(.<) :: Typed Exp -> Typed Exp -> Typed Exp
a .< b = Typed Signed (Op LT a b)

(.<=) :: Typed Exp -> Typed Exp -> Typed Exp
a .<= b = Typed Signed (Op LTE a b)

(.>) :: Typed Exp -> Typed Exp -> Typed Exp
a .> b = Typed Signed (Op GT a b)

(.>=) :: Typed Exp -> Typed Exp -> Typed Exp
a .>= b = Typed Signed (Op GTE a b)

(.==) :: Typed Exp -> Typed Exp -> Typed Exp
a .== b = Typed Signed (Op Eq a b)

(./=) :: Typed Exp -> Typed Exp -> Typed Exp
a ./= b = Typed Signed (Op Neq a b)

(.?) :: Typed Exp -> Typed Exp -> Typed Exp -> Typed Exp
(.?) c a b = Typed (typeOf a) (Cond c a b)

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

-- | b at the same type as that of a.
sameTypeAs :: Typed a -> b -> Typed b
sameTypeAs (Typed t _) x = Typed t x

-- | Maps a type-preserving function over a typed expression.
--   E.g. @not_ = mapTyped Not :: Typed Exp -> Typed Exp@.
mapTyped :: (Typed a -> b) -> Typed a -> Typed b
mapTyped f tx@(Typed t x) = Typed t (f tx)

not_ :: Typed Exp -> Typed Exp
not_ = mapTyped Not

instance Functor Typed where
  fmap f (Typed t a) = Typed t (f a)

type Param = Typed Id

-- | A variable declaration with optional initialization.
data Decl = Decl
  { declType :: !Type
  , declId   :: !Id
  , declInit :: !(Maybe Exp)
  }

-- | A typed JavaScript statement.
data Stmt
  = !Id :=  !(Typed Exp)
  | DeclStm !Decl
  | Inc     !(Typed Id)
  | Dec     !(Typed Id)
  | Ret     !(Typed Exp)
  | Block   ![Stmt]
  | If      !(Typed Exp) !Stmt !(Maybe Stmt)
  | Forever !Stmt
  | For     !Stmt !(Typed Exp) !Stmt !Stmt
  | Break
  | Assert  !(Typed Exp) !String

data Func = Func
  { funParams :: [Param]
  , funLocals :: [Decl]
  , funBody   :: [Stmt]
  }

jsNull :: Typed Exp
jsNull = Typed Unsigned (Lit $ -1)

jsTrue :: Typed Exp
jsTrue = Typed Signed (Lit 1)

jsFalse :: Typed Exp
jsFalse = Typed Signed (Lit 0)
