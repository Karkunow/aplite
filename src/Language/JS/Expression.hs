{-# LANGUAGE CPP #-}
-- | Compile @imperative-edsl@ expressions into JS.
module Language.JS.Expression where
import Language.Syntactic hiding (Typed)
import Language.Syntactic.Functional (Denotation)
import Data.Proxy
import Language.Syntactic hiding (Typed)
import Data.Typeable
import qualified Haste.JSString as S

import Data.Int
import Data.Word
import Data.Maybe
import Data.Bits

-- import Language.Embedded.CExp
import Language.JS.Monad
import Language.JS.CompExp
import Language.JS.Syntax hiding (
    Op, Lit, GE, LE, GT, LT, Neq, Eq, Cast, Cond, Fun, typeOf
  )
import qualified Language.JS.Syntax as JS

--------------------------------------------------------------------------------
-- * Types
--------------------------------------------------------------------------------

-- | Types supported by JS
class (Show a, Eq a, Typeable a, ToJSExp a) => JSType a
  where
    jsType :: proxy a -> Type

instance JSType Bool   where jsType _ = Signed
instance JSType Int    where jsType _ = Signed
instance JSType Int32  where jsType _ = Signed
instance JSType Word   where jsType _ = Unsigned
instance JSType Word32 where jsType _ = Unsigned
instance JSType Double where jsType _ = Double

#if MIN_VERSION_syntactic(3,0,0)
-- instance ShowClass JSType where showClass _ = "JSType"

pJSType :: Proxy JSType
pJSType = Proxy

{-
deriveWitness ''JSType ''BoolType
deriveWitness ''JSType ''FloatType
deriveWitness ''JSType ''DoubleType
deriveWitness ''JSType ''IntWordType

derivePWitness ''JSType ''BoolType
derivePWitness ''JSType ''FloatType
derivePWitness ''JSType ''DoubleType
derivePWitness ''JSType ''IntWordType
-}

{-
instance PWitness JSType CharType t
instance PWitness JSType ListType t
instance PWitness JSType TupleType t
instance PWitness JSType FunType t
-}
#endif

-- | Return whether the type of the expression is a floating-point numeric type
isFloat :: forall a . JSType a => CExp a -> Bool
isFloat a = t == typeOf (undefined :: Float) || t == typeOf (undefined :: Double)
  where
    t = typeOf (undefined :: a)

-- | Return whether the type of the expression is a non-floating-point type
isExact :: JSType a => CExp a -> Bool
isExact = not . isFloat

-- | Return whether the type of the expression is a non-floating-point type
isExact' :: JSType a => ASTF T a -> Bool
isExact' = isExact . CExp

--------------------------------------------------------------------------------
-- * Expressions
--------------------------------------------------------------------------------

data Unary a
  where
    UnNeg :: Num a => Unary (a -> a)
    UnNot :: Unary (Bool -> Bool)

evalUnary :: Unary a -> a
evalUnary UnNeg = negate
evalUnary UnNot = not

data Binary a
  where
    BiAdd     :: Num a              => Binary (a -> a -> a)
    BiSub     :: Num a              => Binary (a -> a -> a)
    BiMul     :: Num a              => Binary (a -> a -> a)
    BiDiv     :: Fractional a       => Binary (a -> a -> a)
    BiFmod    :: RealFrac a         => Binary (a -> a -> a)
    BiQuot    :: Integral a         => Binary (a -> a -> a)
    BiRem     :: Integral a         => Binary (a -> a -> a)
    BiAnd     ::                       Binary (Bool -> Bool -> Bool)
    BiOr      ::                       Binary (Bool -> Bool -> Bool)
    BiEq      :: JSType a           => Binary (a -> a -> Bool)
    BiNEq     :: JSType a           => Binary (a -> a -> Bool)
    BiLt      :: (Ord a, JSType a)  => Binary (a -> a -> Bool)
    BiGt      :: (Ord a, JSType a)  => Binary (a -> a -> Bool)
    BiLe      :: (Ord a, JSType a)  => Binary (a -> a -> Bool)
    BiGe      :: (Ord a, JSType a)  => Binary (a -> a -> Bool)
    BiBitAnd  :: (Bits a, JSType a) => Binary (a -> a -> a)
    BiBitOr   :: (Bits a, JSType a) => Binary (a -> a -> a)
    BiBitXor  :: (Bits a, JSType a) => Binary (a -> a -> a)
    BiBitShl  :: (Bits a, JSType a) => Binary (a -> Int -> a)
    BiBitShr  :: (Bits a, JSType a) => Binary (a -> Int -> a)
    BiBitShrL :: (Bits a, JSType a) => Binary (a -> Int -> a)

evalBinary :: Binary a -> a
evalBinary BiAdd    = (+)
evalBinary BiSub    = (-)
evalBinary BiMul    = (*)
evalBinary BiDiv    = (/)
evalBinary BiFmod   = evalFmod
evalBinary BiQuot   = quot
evalBinary BiRem    = rem
evalBinary BiAnd    = (&&)
evalBinary BiOr     = (||)
evalBinary BiEq     = (==)
evalBinary BiNEq    = (/=)
evalBinary BiLt     = (<)
evalBinary BiGt     = (>)
evalBinary BiLe     = (<=)
evalBinary BiGe     = (>=)
evalBinary BiBitAnd = (.&.)
evalBinary BiBitOr  = (.|.)
evalBinary BiBitXor = xor
evalBinary BiBitShl = shiftL
evalBinary BiBitShr = shiftR

evalFmod :: RealFrac a => a -> a -> a
evalFmod x y = x - (y * (fromIntegral $ (truncate (x/y) :: Integer)))

binaryOp :: Binary a -> BinOp
binaryOp BiAdd     = JS.Add
binaryOp BiSub     = JS.Sub
binaryOp BiMul     = JS.Mul
binaryOp BiDiv     = JS.Div
binaryOp BiFmod    = JS.Mod
binaryOp BiQuot    = JS.Div
binaryOp BiRem     = JS.Mod
binaryOp BiAnd     = JS.And
binaryOp BiOr      = JS.Or
binaryOp BiEq      = JS.Eq
binaryOp BiNEq     = JS.Neq
binaryOp BiLt      = JS.LT
binaryOp BiGt      = JS.GT
binaryOp BiLe      = JS.LTE
binaryOp BiGe      = JS.GTE
binaryOp BiBitAnd  = JS.BitAnd
binaryOp BiBitOr   = JS.BitOr
binaryOp BiBitXor  = JS.BitXor
binaryOp BiBitShl  = JS.Shl
binaryOp BiBitShr  = JS.ShrA
binaryOp BiBitShrL = JS.ShrL

-- | Syntactic symbols for C
data Sym sig
  where
    -- Literal
    Lit   :: String -> a -> Sym (Full a)
    -- Predefined constant. First argument is a list of supporting C includes.
    Const :: [String] -> String -> a -> Sym (Full a)
    -- Function. First argument is a list of supporting C includes.
    Fun   ::
#if MIN_VERSION_syntactic(3,0,0)
             Signature sig =>
#endif
             [String] -> String -> Denotation sig -> Sym sig
    -- Unary operator
    UOp   :: Unary (a -> b) -> Sym (a :-> Full b)
    -- Binary operator
    Op    :: Binary (a -> b -> c) -> Sym (a :-> b :-> Full c)
    -- Type casting (ignored when generating code)
    Cast  :: (a -> b) -> Sym (a :-> Full b)
    -- Conditional
    Cond  :: Sym (Bool :-> a :-> a :-> Full a)
    -- Variable (only for compilation)
    Var   :: VarId -> Sym (Full a)
    -- Unsafe array indexing
--    ArrIx :: (Integral i, Ix i) => IArr i a -> Sym (i :-> Full a)

data T sig
  where
    T :: JSType (DenResult sig) => { unT :: Sym sig } -> T sig

-- | C expression
newtype CExp a = CExp {unCExp :: ASTF T a}

instance Syntactic (CExp a)
  where
    type Domain (CExp a)   = T
    type Internal (CExp a) = a
    desugar = unCExp
    sugar   = CExp

evalSym :: Sym sig -> Denotation sig
evalSym (Lit _ a)     = a
evalSym (Const _ _ a) = a
evalSym (Fun _ _ f)   = f
evalSym (UOp uop)     = evalUnary uop
evalSym (Op bop)      = evalBinary bop
evalSym (Cast f)      = f
evalSym Cond          = \c t f -> if c then t else f
{-
evalSym (ArrIx (IArrEval arr)) = \i ->
    if i<l || i>h
      then error $ "index "
                ++ show (toInteger i)
                ++ " out of bounds "
                ++ show (toInteger l, toInteger h)
      else arr!i
  where
    (l,h) = bounds arr
-}
evalSym (Var v) = error $ "evalCExp: cannot evaluate variable " ++ S.unpack v

-- | Evaluate an expression
evalCExp :: CExp a -> a
evalCExp (CExp e) = go e
  where
    go :: AST T sig -> Denotation sig
    go (Sym (T s)) = evalSym s
    go (f :$ a)    = go f $ go a

instance EvalExp CExp
  where
    evalExp  = evalCExp

instance CompJSExp CExp
  where
    compExp  = compJSExp
    compType = pure . jsType

instance FreeExp CExp
  where
    type VarPred CExp = JSType
    valExp a = CExp $ Sym $ T $ Lit (show a) a
    varExp   = CExp . Sym . T . Var

-- | One-level constant folding: if all immediate sub-expressions are literals,
-- the expression is reduced to a single literal
constFold :: CExp a -> CExp a
constFold = CExp . match go . unCExp
  where
    go :: T sig -> Args (AST T) sig -> AST T (Full (DenResult sig))
    go (T s) as = res
      where
        e   = appArgs (Sym $ T s) as
        res = if and $ listArgs (isJust . viewLit . CExp) as
                then unCExp $ value $ evalCExp $ CExp e
                else e
  -- Deeper constant folding would require a way to witness `Show` for arbitrary
  -- sub-expressions. This is certainly doable, but seems to complicate things
  -- for not much gain (currently).

castAST :: forall a b . Typeable b => ASTF T a -> Maybe (ASTF T b)
castAST a = simpleMatch go a
  where
    go :: (DenResult sig ~ a) => T sig -> Args (AST T) sig -> Maybe (ASTF T b)
    go (T _) _ = gcast a

-- | Get the value of a literal expression
viewLit :: CExp a -> Maybe a
viewLit (CExp (Sym (T (Lit _ a)))) = Just a
viewLit _ = Nothing

pattern LitP a      <- CExp (Sym (T (Lit _ a)))
pattern LitP' a     <- Sym (T (Lit _ a))
pattern NonLitP     <- (viewLit -> Nothing)
pattern NonLitP'    <- (CExp -> (viewLit -> Nothing))
pattern OpP op a b  <- CExp (Sym (T (Op op)) :$ a :$ b)
pattern OpP' op a b <- Sym (T (Op op)) :$ a :$ b
pattern UOpP op a   <- CExp (Sym (T (UOp op)) :$ a)
pattern UOpP' op a  <- Sym (T (UOp op)) :$ a



--------------------------------------------------------------------------------
-- * User interface
--------------------------------------------------------------------------------

-- | Construct a literal expression
value :: JSType a => a -> CExp a
value a = CExp $ Sym $ T $ Lit (show a) a

-- | Predefined constant
constant :: JSType a
    => [String]  -- ^ Supporting C includes
    -> String    -- ^ Name of constant
    -> a         -- ^ Value of constant
    -> CExp a
constant incls const val = CExp $ Sym $ T $ Const incls const val

-- | Create a named variable
variable :: JSType a => VarId -> CExp a
variable = CExp . Sym . T . Var

true, false :: CExp Bool
true  = constant ["<stdbool.h>"] "true" True
false = constant ["<stdbool.h>"] "false" False

instance (Num a, Ord a, JSType a) => Num (CExp a)
  where
    fromInteger = value . fromInteger

    LitP 0 + b | isExact b = b
    a + LitP 0 | isExact a = a
    a@(LitP _) + b@NonLitP | isExact a = b+a  -- Move literals to the right
    OpP BiAdd a (LitP' b) + LitP c | isExact' a = CExp a + value (b+c)
    OpP BiSub a (LitP' b) + LitP c | isExact' a = CExp a + value (c-b)
    a + LitP b | b < 0, isExact a = a - value (negate b)
    a + b = constFold $ sugarSym (T $ Op BiAdd) a b

    LitP 0 - b | isExact b = negate b
    a - LitP 0 | isExact a = a
    a@(LitP _) - b@NonLitP | isExact a = negate b - negate a  -- Move literals to the right
    OpP BiAdd a (LitP' b) - LitP c | isExact' a = CExp a + value (b-c)
    OpP BiSub a (LitP' b) - LitP c | isExact' a = CExp a - value (b+c)
    a - LitP b | b < 0, isExact a = a + value (negate b)
    a - b = constFold $ sugarSym (T $ Op BiSub) a b

    LitP 0 * b | isExact b = value 0
    a * LitP 0 | isExact a = value 0
    LitP 1 * b | isExact b = b
    a * LitP 1 | isExact a = a
    a@(LitP _) * b@NonLitP | isExact a = b*a  -- Move literals to the right
    OpP BiMul a (LitP' b) * LitP c | isExact' a = CExp a * value (b*c)
    a * b = constFold $ sugarSym (T $ Op BiMul) a b

    negate (UOpP UnNeg a)  | isExact' a = CExp a
    negate (OpP BiAdd a b) | isExact' a = negate (CExp a) - CExp b
    negate (OpP BiSub a b) | isExact' a = CExp b - CExp a
    negate (OpP BiMul a b) | isExact' a = CExp a * negate (CExp b)
      -- Negate the right operand, because literals are moved to the right
      -- in multiplications
    negate a = constFold $ sugarSym (T $ UOp UnNeg) a

    abs    = constFold . sugarSym (T $ Fun ["<math.h>"] "abs" abs)
    signum = error "signum not supported for CExp!" -- constFold . sugarSym (T $ Fun ["<math.h>"] "sign" signum)

instance (Ord a, Num a, JSType a, Bits a) => Bits (CExp a) where
  a .&. b        = constFold $ sugarSym (T $ Op BiBitAnd) a b
  a .|. b        = constFold $ sugarSym (T $ Op BiBitOr) a b
  a `xor` b      = constFold $ sugarSym (T $ Op BiBitXor) a b
  a `shiftL` b   = constFold $ sugarSym (T $ Op BiBitShl) a (fromIntegral b :: CExp Int)
  a `shiftR` b   = constFold $ sugarSym (T $ Op BiBitShr) a (fromIntegral b :: CExp Int)
  bit x          = 1 `shiftL` x
  bitSize _      = 32
  bitSizeMaybe _ = Just 32
  isSigned _     = isSigned (undefined :: a)
  testBit _ _    = error "testBit: unsupported"
  popCount _     = error "popCount: unsupported"
  complement     = xor 0xffffffff

shiftRL :: (Ord a, Num a, JSType a, Bits a) => CExp a -> Int -> CExp a
shiftRL a b =
  constFold $ sugarSym (T $ Op BiBitShrL) a (fromIntegral b :: CExp Int)
infixl 8 `shiftRL`

instance (Fractional a, Ord a, JSType a) => Fractional (CExp a)
  where
    fromRational = value . fromRational
    a / b = constFold $ sugarSym (T $ Op BiDiv) a b

    recip = error "recip not implemented for CExp"

instance (Floating a, Ord a, JSType a) => Floating (CExp a)
  where
    pi     = value pi
    a ** b = constFold $ sugarSym (T $ Fun ["<math.h>"] "pow" (**)) a b
    sin a  = constFold $ sugarSym (T $ Fun ["<math.h>"] "sin" sin) a
    cos a  = constFold $ sugarSym (T $ Fun ["<math.h>"] "cos" cos) a

-- | Integer division truncated toward zero
quot_ :: (Integral a, JSType a) => CExp a -> CExp a -> CExp a
quot_ (LitP 0) b = 0
quot_ a (LitP 1) = a
quot_ a b
    | a == b     = 1
quot_ a b        = constFold $ sugarSym (T $ Op BiQuot) a b

-- | Integer remainder satisfying
--
-- > (x `quot_` y)*y + (x #% y) == x
(#%) :: (Integral a, JSType a) => CExp a -> CExp a -> CExp a
LitP 0 #% _          = 0
_      #% LitP 1     = 0
a      #% b | a == b = 0
a      #% b          = constFold $ sugarSym (T $ Op BiRem) a b

fmod :: CExp Double -> CExp Double -> CExp Double
fmod a b = constFold $ sugarSym (T $ Op BiFmod) a b

round_ :: (RealFrac a, JSType a) => CExp a -> CExp a
round_ = constFold . sugarSym (T $ Fun ["<math.h>"] "round" (fromInteger . round))

floor_ :: (RealFrac a, JSType a) => CExp a -> CExp a
floor_ = constFold . sugarSym (T $ Fun ["<math.h>"] "floor" (fromInteger . floor))

ceiling_ :: (RealFrac a, JSType a) => CExp a -> CExp a
ceiling_ = constFold . sugarSym (T $ Fun ["<math.h>"] "ceiling" (fromInteger . ceiling))

sqrt_ :: (Floating a, JSType a) => CExp a -> CExp a
sqrt_ = constFold . sugarSym (T $ Fun ["<math.h>"] "sqrt" sqrt)

f2n :: (RealFrac a, Num b, JSType b) => CExp a -> CExp b
f2n a = constFold $ sugarSym (T $ Cast (fromInteger . truncate)) a

-- | Integral type casting
i2n :: (Integral a, Num b, JSType b) => CExp a -> CExp b
i2n a = constFold $ sugarSym (T $ Cast (fromInteger . toInteger)) a

-- | Cast integer to 'Bool'
i2b :: Integral a => CExp a -> CExp Bool
i2b a = constFold $ sugarSym (T $ Cast (/=0)) a

-- | Boolean negation
not_ :: CExp Bool -> CExp Bool
not_ (UOpP UnNot a)  = CExp a
not_ (OpP BiEq a b)  = CExp a #!= CExp b
not_ (OpP BiNEq a b) = CExp a #== CExp b
not_ (OpP BiLt a b)  = CExp a #>= CExp b
not_ (OpP BiGt a b)  = CExp a #<= CExp b
not_ (OpP BiLe a b)  = CExp a #> CExp b
not_ (OpP BiGe a b)  = CExp a #< CExp b
not_ a = constFold $ sugarSym (T $ UOp UnNot) a

-- | Logical and
(#&&) :: CExp Bool -> CExp Bool -> CExp Bool
LitP True  #&& b          = b
LitP False #&& b          = false
a          #&& LitP True  = a
a          #&& LitP False = false
a          #&& b          = constFold $ sugarSym (T $ Op BiAnd) a b

-- | Logical or
(#||) :: CExp Bool -> CExp Bool -> CExp Bool
LitP True  #|| b          = true
LitP False #|| b          = b
a          #|| LitP True  = true
a          #|| LitP False = a
a          #|| b          = constFold $ sugarSym (T $ Op BiOr) a b

-- | Equality
(#==) :: (Eq a, JSType a) => CExp a -> CExp a -> CExp Bool
a #== b
    | a == b, isExact a = true
    | otherwise         = constFold $ sugarSym (T $ Op BiEq) a b

-- | In-equality
(#!=) :: (Eq a, JSType a) => CExp a -> CExp a -> CExp Bool
a #!= b
    | a == b, isExact a = false
    | otherwise         = constFold $ sugarSym (T $ Op BiNEq) a b

(#<) :: (Ord a, JSType a) => CExp a -> CExp a -> CExp Bool
a #< b
    | a == b, isExact a = false
    | otherwise         = constFold $ sugarSym (T $ Op BiLt) a b

(#>) :: (Ord a, JSType a) => CExp a -> CExp a -> CExp Bool
a #> b
    | a == b, isExact a = false
    | otherwise         = constFold $ sugarSym (T $ Op BiGt) a b

(#<=) :: (Ord a, JSType a) => CExp a -> CExp a -> CExp Bool
a #<= b
    | a == b, isExact a = true
    | otherwise         = constFold $ sugarSym (T $ Op BiLe) a b

(#>=) :: (Ord a, JSType a) => CExp a -> CExp a -> CExp Bool
a #>= b
    | a == b, isExact a = true
    | otherwise         = constFold $ sugarSym (T $ Op BiGe) a b

infix 4 #==, #!=, #<, #>, #<=, #>=

-- | Conditional expression
cond :: JSType a
    => CExp Bool  -- ^ Condition
    -> CExp a     -- ^ True branch
    -> CExp a     -- ^ False branch
    -> CExp a
cond (LitP c) t f = if c then t else f
cond c t f
    | t == f = t
cond (UOpP UnNot a) t f = cond (CExp a) f t
cond c t f = constFold $ sugarSym (T Cond) c t f

-- | Condition operator; use as follows:
--
-- > cond1 ? a $
-- > cond2 ? b $
-- > cond3 ? c $
-- >         default
(?) :: JSType a
    => CExp Bool  -- ^ Condition
    -> CExp a     -- ^ True branch
    -> CExp a     -- ^ False branch
    -> CExp a
(?) = cond

infixl 1 ?

{-
-- | Array indexing
(#!) :: (JSType a, Integral i, Ix i) => IArr i a -> CExp i -> CExp a
arr #! i = sugarSym (T $ ArrIx arr) i
-}

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

#if MIN_VERSION_syntactic(3,0,0)
instance Render Sym
  where
    renderSym (Lit a _)      = a
    renderSym (Const _ a _)  = a
    renderSym (Fun _ name _) = name
    renderSym (UOp UnNeg)    = show Neg
    renderSym (UOp UnNot)    = show Not
    renderSym (Op op)        = show $ binaryOp op
    renderSym (Cast _)       = "cast"
    renderSym (Var v)        = S.unpack v
    renderArgs = renderArgsSmart

instance Equality Sym
  where
    equal = equalDefault
    hash  = hashDefault

instance StringTree Sym

instance Symbol Sym where
  symSig (Lit _ _) = signature
  symSig (Const _ _ _) = signature
  symSig (Fun _ _ _) = signature
  symSig (UOp _) = signature
  symSig (Op _) = signature
  symSig (Cast _) = signature
  symSig Cond = signature
  symSig (Var _) = signature
  symSig _ = error "add ArrIx here!"

instance Symbol T where symSig (T s) = symSig s

instance Render T
  where
    renderSym (T s)     = renderSym s
    renderArgs as (T s) = renderArgs as s

instance Equality T
  where
    equal (T s) (T t) = equal s t
    hash (T s)        = hash s

instance StringTree T
  where
    stringTreeSym as (T s) = stringTreeSym as s

#else

instance Semantic Sym
  where
    semantics (Lit s a)      = Sem s a
    semantics (Const _ s a)  = Sem s a
    semantics (Fun _ name f) = Sem name f
    semantics (UOp uop)      = Sem (show $ unaryOp uop) (evalUnary uop)
    semantics (Op bop)       = Sem (show $ binaryOp bop) (evalBinary bop)
    semantics (Cast f)       = Sem "cast" f
    semantics (Var v)        = Sem v $ error $ "evaluating free variable: " ++ v

instance Equality Sym
  where
    equal    = equalDefault
    exprHash = exprHashDefault

instance Semantic T
  where
    semantics (T s) = semantics s

instance Equality T
  where
    equal (T s) (T t) = equal s t
    exprHash (T s)    = exprHash s

#endif

deriving instance Eq (CExp a)
  -- Must be placed here due to the sequential dependencies introduced by
  -- Template Haskell


compJSExp :: forall a. CExp a -> JSGen (Typed Exp)
compJSExp = simpleMatch (\(T s) -> go s) . unCExp
  where
    compJSExp' :: ASTF T b -> JSGen (Typed Exp)
    compJSExp' = compJSExp . CExp

    go :: forall sig. JSType (DenResult sig)
       => Sym sig
       -> Args (AST T) sig
       -> JSGen (Typed Exp)
    go a b = goWithType a b $ jsType (Proxy :: Proxy (DenResult sig))

    genArgs :: forall sig. Args (AST T) sig -> JSGen [Typed Exp]
    genArgs (a :* as) = do
      a' <- compJSExp' a
      as' <- genArgs as
      pure (a':as')
    genArgs _ = pure []

    goWithType :: forall sig. JSType (DenResult sig)
       => Sym sig
       -> Args (AST T) sig
       -> Type
       -> JSGen (Typed Exp)
    goWithType (Var v) Nil t = do
      genVar t v
    goWithType (Lit _ x) Nil t = do
      pure (typed t (toJSExp x))
    goWithType (Const _ _ c) Nil t = do
      pure (typed t (toJSExp c))
    goWithType (Fun _ f _) args t = do
      args' <- genArgs args
      pure (typed t (JS.Call (S.pack f) args'))
    goWithType (UOp op) (a :* Nil) t = do
      a' <- compJSExp' a
      pure $ case op of
        UnNot -> typed t (Not a')
        UnNeg -> typed t (Neg a')
    goWithType (Op op) (a :* b :* Nil) t = do
      a' <- compJSExp' a
      b' <- compJSExp' b
      pure (typed t (JS.Op (binaryOp op) a' b'))
    goWithType s@(Cast f) (a :* Nil) t = do
      a' <- compJSExp' a
      pure (typed t (JS.Cast t a'))
    goWithType Cond (c :* t :* f :* Nil) _ = do
      c' <- compJSExp' c
      t' <- compJSExp' t
      f' <- compJSExp' f
      pure (c' .? t' $ f')

instance JSType a => ReturnValue (CExp a) where
  returnStmt x = Just $ Ret <$> compJSExp x
