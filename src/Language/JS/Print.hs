module Language.JS.Print where
import Language.JS.Syntax
import Control.Monad.Reader
import Control.Monad
import Data.List

data CodeStyle
  = ASMJS
  | JavaScript
    deriving Eq

data CodeHeader
  = StrictHeader
  | ASMHeader
  | NoHeader
    deriving Eq

data CodeTuning = CodeTuning
  { codeStyle  :: CodeStyle
  , headerDecl :: CodeHeader
  , useHeap    :: Bool
  }

defaultTuning :: CodeTuning
defaultTuning = CodeTuning
  { codeStyle  = JavaScript
  , headerDecl = NoHeader
  , useHeap    = False
  }

type Printer = Reader CodeTuning [String]

whenCfg :: (CodeTuning -> Bool) -> Reader CodeTuning () -> Reader CodeTuning ()
whenCfg p m = do
  env <- ask
  when (p env) m

printJS :: PrintJS a => CodeTuning -> a -> String
printJS cfg = concat . flip runReader cfg . fromJS

class PrintJS a where
  fromJS :: a -> Printer

instance PrintJS Id where
  fromJS (MkId n) = pure ['v' : show n]

instance PrintJS a => PrintJS (Typed a) where
  fromJS (Typed t x) = do
      codeStyle <$> ask >>= \cs -> case cs of
        ASMJS      -> typed t (paren (fromJS x))
        JavaScript -> fromJS x

instance PrintJS Decl where
  fromJS (Decl t n mx) = do
      codeStyle <$> ask >>= \cs -> case cs of
        ASMJS      -> str "var " .+. fromJS n .+. asmInit
        JavaScript -> str "var " .+. fromJS n .+. jsInit
    where
      asmInit :: Printer
      asmInit = str "=" .+. typed t (maybe (str "0") fromJS mx)
      jsInit = maybe (pure []) (\x -> str "=" .+. fromJS x) mx

instance PrintJS BinOp where
  fromJS op = str (show op)

instance PrintJS Exp where
  fromJS (Id n)       = fromJS n
  fromJS (Op op a b)  = fromJS a .+. fromJS op .+. fromJS b
  fromJS (Lit x)
    | isIntegral x    = str (show (truncate x))
    | otherwise       = str (show x)
  fromJS (Neg x)      = str "-" .+. fromJS x
  fromJS (Not x)      = str "!" .+. fromJS x
  -- coercion performed by mandatory type annotation anyway
  fromJS (Cast _ x)   = fromJS x
  fromJS (Cond _ _ _) = error "TODO: ternary operator not supported in asm.js!"

isIntegral :: Double -> Bool
isIntegral x = x == fromIntegral (truncate x :: Int)

instance PrintJS Stmt where
  fromJS (a := b) =
    fromJS a .+. str "=" .+. fromJS b
  fromJS (DeclStm d) =
    fromJS d
  fromJS (Inc x) = do
    codeStyle <$> ask >>= \cs -> case cs of
      ASMJS ->
        typed (typeOf x) $ fromJS x .+. str "=" .+. fromJS x .+. str "+1"
      JavaScript ->
        str "++" .+. fromJS x
  fromJS (Dec x) = do
    codeStyle <$> ask >>= \cs -> case cs of
      ASMJS ->
        typed (typeOf x) $ fromJS x .+. str "=" .+. fromJS x .+. str "-1"
      JavaScript ->
        str "--" .+. fromJS x
  fromJS (Ret x) =
    str "return " .+. fromJS x
  fromJS (Block ss) = do
    ss' <- mapM fromJS ss
    str "{" .+. str (concat $ intercalate [";"] ss') .+. str ";}"
  fromJS (If e a mb) = do
    ifPart <- str "if(" .+. fromJS e .+. str ")" .+. fromJS a
    case mb of
      Just b -> pure ifPart .+. str "else" .+. fromJS b
      _      -> pure ifPart
  fromJS (Forever s) =
    str "while(1)" .+. fromJS s
  fromJS (For i cond step body) =
    foldr (.+.) (pure [])
      [ str "for(", fromJS i, str ";", fromJS cond, str ";", fromJS step
      , str ")"
      , fromJS body
      ]
  fromJS Break =
    str "break"
  fromJS (Assert e s) =
    error "TODO: assert"

-- TODO: wrap this in proper ASM.js module when appropriate
instance PrintJS Func where
  fromJS (Func params locals body) = do
    cfg <- ask
    params' <- concat . intercalate [","] <$> mapM (fromJS . untyped) params
    let argdecls = case codeStyle cfg of
          ASMJS      -> [n := Typed t (Lit 0) | Typed t n <- params]
          JavaScript -> []
    foldr (.+.) (pure [])
      [ str "function(", str params', str ")"
      , fromJS (Block (argdecls ++ map DeclStm locals ++ body))
      ]

str :: String -> Printer
str s = pure [s]

(.+.) :: Printer -> Printer -> Printer
(.+.) = liftM2 (++)
infixr 7 .+.

typed :: Type -> Printer -> Printer
typed Double m   = paren $ m >>= \x -> pure ("+":x)
typed Signed m   = paren $ m >>= \x -> pure (x ++ ["|0"])
typed Unsigned m = paren $ m >>= \x -> pure (x ++ [">>>0"])

paren :: Printer -> Printer
paren m = m >>= \x -> pure $ ["("] ++ x ++ [")"]
