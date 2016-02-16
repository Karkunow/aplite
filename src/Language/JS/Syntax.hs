-- | Restricted JavaScript syntax, suitable for generating ASM.js as well as
--   regular JS.
module Language.JS.Syntax
  ( module Language.JS.BinOps
  , ToJSExp (..)
  , Type (..), isPtrTy
  , Literal (..), Id (..), jsNull
  , Decl (..) , Param
  , Typed (..), Exp (..), Stmt (..)
  ) where
import Language.JS.BinOps
import Data.Int
import Data.Word

class ToJSExp a where
  toJSExp :: a -> Exp

instance ToJSExp Bool   where toJSExp = Lit . BoolLit
instance ToJSExp Int32  where toJSExp = Lit . SignedLit
instance ToJSExp Word32 where toJSExp = Lit . UnsignedLit
instance ToJSExp Double where toJSExp = Lit . DoubleLit

-- | Type of a JavaScript expression.
data Type
  = Signed
  | Unsigned
  | Double
  | Bool
  | Arr Type

isPtrTy :: Type -> Bool
isPtrTy (Arr _) = True
isPtrTy _       = False

data Literal
  = SignedLit   !Int32
  | UnsignedLit !Word32
  | DoubleLit   !Double
  | BoolLit     !Bool
  | PointerLit  !Int

-- | A JavaScript identifier.
newtype Id = MkId {idInteger :: Integer}
  deriving (Show, Eq, Enum)

-- | An untyped JavaScript expression with typed subexpressions.
data Exp
  = Id   !Id
  | Op   !BinOp !(Typed Exp) !(Typed Exp)
  | Lit  !Literal
  | Neg  !(Typed Exp)
  | Not  !(Typed Exp)
  | Cast !Type !(Typed Exp)
  | Cond !Exp !(Typed Exp) !(Typed Exp)

-- | Type annotation for JavaScript AST nodes.
data Typed a = Typed
  { typeOf  :: !Type
  , untyped :: !a
  }

type Param = Typed Id

-- | A variable declaration with optional initialization.
data Decl = Decl
  { declType :: !Type
  , declId   :: !Id
  , declInit :: !(Maybe Exp)
  }

-- | A typed JavaScript statement.
data Stmt
  = !Id := !(Typed Exp)
  | Ret !(Typed Exp)

jsNull :: Typed Exp
jsNull = Typed (Unsigned) (Lit $ PointerLit (-1))
