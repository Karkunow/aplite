module Language.JS.Print where
import Language.JS.Syntax
import Control.Monad.Reader
import Control.Monad
import Data.List

{-# WARNING CodeStyle "TODO: ~~ for double -> signed conversion in ASM.js" #-}

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

data PrintEnv = PrintEnv
  { indent :: String
  , tuning :: CodeTuning
  }

type PrintM = Reader PrintEnv
type Printer = PrintM [String]

printJS :: PrintJS a => CodeTuning -> a -> String
printJS t = concat . flip runReader (mkEnv t) . fromJS
  where
    mkEnv t = PrintEnv
      { indent = ""
      , tuning = t
      }

class PrintJS a where
  needsParen :: a -> PrintM Bool
  needsParen _ = pure True
  
  fromJS :: a -> Printer

instance PrintJS Id where
  fromJS (MkId n)     = pure ['v' : show n]
  fromJS (External n) = pure [show n]

instance PrintJS a => PrintJS (Typed a) where
  needsParen (Typed _ x) = do
    codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS -> pure True
      _     -> needsParen x

  fromJS (Typed t x) = do
      codeStyle . tuning <$> ask >>= \cs -> case cs of
        ASMJS      -> typed t (parenIfNecessary x)
        JavaScript -> fromJS x

parenIfNecessary :: PrintJS a => a -> Printer
parenIfNecessary x = do
  useParens <- needsParen x
  if useParens
    then paren (fromJS x)
    else fromJS x

instance PrintJS Decl where
  fromJS (Decl t n mx) = do
      codeStyle . tuning <$> ask >>= \cs -> case cs of
        ASMJS      -> str "var " .+. fromJS n .+. asmInit
        JavaScript -> str "var " .+. fromJS n .+. jsInit
    where
      asmInit :: Printer
      asmInit = str "=" .+. typed t (maybe (str "0") fromJS mx)
      jsInit = maybe (pure []) (\x -> str "=" .+. fromJS x) mx

instance PrintJS BinOp where
  fromJS op = str (show op)

instance PrintJS Exp where
  needsParen (Id _)  = pure False
  needsParen (Lit _) = pure False
  needsParen _       = pure True

  fromJS (Id n)       = fromJS n
  fromJS (Op op a b)  = parenIfNecessary a .+. fromJS op .+. parenIfNecessary b
  fromJS (Lit x)
    | isIntegral x    = str (show (truncate x))
    | otherwise       = str (show x)
  fromJS (Neg x)      = str "-" .+. fromJS x
  fromJS (Not x)      = str "!" .+. fromJS x
  -- TODO: ~~ for conversion double -> signed!
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
    codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS ->
        typed (typeOf x) $ fromJS x .+. str "=" .+. fromJS x .+. str "+1"
      JavaScript ->
        str "++" .+. fromJS x
  fromJS (Dec x) = do
    codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS ->
        typed (typeOf x) $ fromJS x .+. str "=" .+. fromJS x .+. str "-1"
      JavaScript ->
        str "--" .+. fromJS x
  fromJS (Ret x) =
    str "return " .+. fromJS x
  fromJS (Block ss) = do
    ind <- indent <$> ask
    let ind' = "  " ++ ind
    ss' <- local (\env -> env {indent = ind'}) $ mapM fromJS ss
    pure ["{\n", ind'] .+. pure (intercalate [";\n", ind'] ss')
                      .+. pure [";\n", ind, "}"]
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
    cfg <- tuning <$> ask
    params' <- concat . intercalate [","] <$> mapM (fromJS . untyped) params
    let argdecls = case codeStyle cfg of
          ASMJS      -> [n := Typed t (Lit 0) | Typed t n <- params]
          JavaScript -> []
    foldr (.+.) (pure [])
      [ str "(function(", str params', str ")"
      , fromJS (Block (argdecls ++ map DeclStm locals ++ body))
      , str ")"
      ]

str :: String -> Printer
str s = pure [s]

(.+.) :: Printer -> Printer -> Printer
(.+.) = liftM2 (++)
infixr 7 .+.

typed :: Type -> Printer -> Printer
typed Double m   = m >>= \x -> pure ("+":x)
typed Signed m   = m >>= \x -> pure (x ++ ["|0"])
typed Unsigned m = m >>= \x -> pure (x ++ [">>>0"])

paren :: Printer -> Printer
paren m = m >>= \x -> pure $ ["("] ++ x ++ [")"]
