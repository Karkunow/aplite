{-# LANGUAGE OverloadedStrings #-}
module Language.JS.Print where
import Language.JS.Syntax hiding (typed)
import Control.Monad.Reader
import Control.Monad
import Data.List
import qualified Haste.JSString as S
import Haste (JSString, toJSString)

{-# WARNING CodeStyle "TODO: ~~ for double -> signed conversion in ASM.js" #-}

data CodeStyle
  = ASMJS
  | JavaScript
    deriving (Eq, Show)

data CodeHeader
  = StrictHeader
  | ASMHeader
  | NoHeader
    deriving (Eq, Show)

type HeapSize = Int

data CodeTuning = CodeTuning
  { codeStyle    :: CodeStyle
  , headerDecl   :: CodeHeader
  , explicitHeap :: Maybe HeapSize
  }

defaultTuning :: CodeTuning
defaultTuning = CodeTuning
  { codeStyle    = JavaScript
  , headerDecl   = NoHeader
  , explicitHeap = Nothing
  }

asmjsTuning :: CodeTuning
asmjsTuning = CodeTuning
  { codeStyle    = ASMJS
  , explicitHeap = Just 0x1000
  , headerDecl   = ASMHeader
  }

-- | JavaScript @"use X";@ declaration for the given code header.
headerDeclStr :: CodeHeader -> JSString
headerDeclStr StrictHeader = "\"use strict\";"
headerDeclStr ASMHeader    = "\"use asm\";"
headerDeclStr NoHeader     = ""

data PrintEnv = PrintEnv
  { indent :: JSString
  , tuning :: CodeTuning
  }

type PrintM = Reader PrintEnv
type Printer = PrintM [JSString]

printJS :: PrintJS a => CodeTuning -> a -> JSString
printJS t = wrap t . S.concat . flip runReader env . fromJS
  where
    env = PrintEnv
      { indent = ""
      , tuning = t
      }

-- | Add pre- and post scaffolding to a string of JS based on the current
--   code tuning.
--   TODO: add HOFs to FFI?
wrap :: CodeTuning -> JSString -> JSString
wrap t prog
  | codeStyle t == ASMJS =
    S.concat
      [ "("
      , asmModule t prog
      , ")(window, null, new ArrayBuffer(", heapsize, ")).f"
      ]
  | otherwise =
    prog
  where
    heapsize = maybe "0" toJSString (explicitHeap t)

-- | JSString representation of an ASM.js module.
asmModule :: CodeTuning -> JSString -> JSString
asmModule t prog = S.intercalate "\n"
  [ "function(stdlib, ffi, heap){"
  , headerDeclStr (headerDecl t)
    -- TODO: type specific views into the heap here!
  , stdlibImports
  , "return ({f:(", prog, ")});"
  , "}"
  ]

-- | Import directives for stdlib functions.
stdlibImports :: JSString
stdlibImports = S.intercalate "\n" $ map mkImport std_funs
  where
    mkImport f = S.concat ["var ", stdFunName f, " = "
                          , "stdlib.Math.", stdFunName f, ";"]

class PrintJS a where
  needsParen :: a -> PrintM Bool
  needsParen _ = pure True
  
  fromJS :: a -> Printer

instance PrintJS Param where
  fromJS (Param t n) = fromJS n .+. str " = " .+. typed t (fromJS n)

instance PrintJS Id where
  fromJS (MkId n)     = pure ["v", toJSString n]
  fromJS (External n) = pure [toJSString n]

instance PrintJS (Typed Id) where
  needsParen (Typed _ _) = codeStyle . tuning <$> ask >>= \cs -> case cs of
    ASMJS      -> pure True
    JavaScript -> pure False
  fromJS (Typed _ x) = fromJS x

instance PrintJS (Typed Exp) where
  needsParen (Typed _ _) = codeStyle . tuning <$> ask >>= \cs -> case cs of
    ASMJS      -> pure True
    JavaScript -> pure False

  fromJS (Typed Signed (Op Mul a b)) =
    fromJS $ Typed Signed (Call "imul" [a, b])
  fromJS (Typed Unsigned (Op Mul a b)) =
    fromJS $ Typed Unsigned (Call "imul" [a, b])  
  fromJS (Typed t x) = do
      codeStyle . tuning <$> ask >>= \cs -> case cs of
        ASMJS
          | Lit _ <- x   -> typed t (fromJS x)
          | Id v <- x    -> typed t (fromJS v)
          | otherwise    -> typed t (paren (fromJS x))
        JavaScript       -> fromJS x

parenIfNecessary :: PrintJS a => a -> Printer
parenIfNecessary x = do
  useParens <- needsParen x
  if useParens
    then paren (fromJS x)
    else fromJS x

-- | Literal ASM.js zero with the specified type. Either 0.0, for doubles, or
--   just plain 0, for integral types.
zeroFor :: Type -> JSString
zeroFor Double = "0.0"
zeroFor _      = "0"

instance PrintJS Decl where
  fromJS (Decl t n mx) = do
      codeStyle . tuning <$> ask >>= \cs -> case cs of
        ASMJS      -> str "var " .+. fromJS n .+. asmInit
        JavaScript -> str "var " .+. fromJS n .+. jsInit
    where
      asmInit :: Printer
      asmInit = str " = " .+. maybe (str (zeroFor t)) fromJS mx
      jsInit  = maybe (pure []) (\x -> str " = " .+. fromJS x) mx

instance PrintJS BinOp where
  fromJS op = pure [" ", (toJSString op), " "]

instance PrintJS Exp where
  needsParen (Id _)  = pure False
  needsParen (Lit _) = pure False
  needsParen _       = pure True

  fromJS (Id n)       = fromJS n
  fromJS (Op op a b)  = parenIfNecessary a .+. fromJS op .+. parenIfNecessary b
  fromJS (Lit x)
    | isIntegral x    = str (toJSString (truncate x :: Int))
    | otherwise       = str (toJSString x)
  fromJS (Neg x)      = str "-(" .+. fromJS x .+. str ")"
  fromJS (Not x)      = str "!(" .+. fromJS x .+. str ")"
  fromJS (Cast t x)   = genCast t x
  fromJS (Cond _ _ _) = error "TODO: ternary operator not supported in asm.js!"
  fromJS (Call f as)  = do
    prefix <- codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS      -> pure ""
      JavaScript -> pure "Math."
    as' <- intercalate [","] <$> mapM (fromJS . untyped) as
    pure $ [prefix, toJSString f, "("] ++ as' ++ [")"]

genCast :: Type -> Typed Exp -> Printer
genCast Signed (Typed Double x)   = str "(~~(" .+. fromJS x .+. str "))"
genCast Unsigned (Typed Double x) = str "((~~(" .+. fromJS x .+. str "))>>>0)"
genCast _ x                       = fromJS x

isIntegral :: Double -> Bool
isIntegral x = x == fromIntegral (truncate x :: Int)

instance PrintJS Stmt where
  fromJS (a := b) =
    fromJS a .+. str " = " .+. fromJS b
  fromJS (DeclStm d) =
    fromJS d
  fromJS (ParamStm p) =
    fromJS p
  fromJS (Inc x) = do
    codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS ->
        fromJS x .+. str " = " .+. typed (typeOf x) (paren $ fromJS x .+. str "+1")
      JavaScript ->
        str "++" .+. fromJS x
  fromJS (Dec x) = do
    codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS ->
        fromJS x .+. str " = " .+. typed (typeOf x) (paren $ fromJS x .+. str "-1")
      JavaScript ->
        str "--" .+. fromJS x
  fromJS (Ret x) =
    str "return " .+. fromJS x
  fromJS (Block ss) = do
    ind <- indent <$> ask
    let ind' = S.concat ["  ", ind]
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
    params' <- S.concat . intercalate [","] <$> mapM (fromJS . paramName) params
    let argdecls = case codeStyle cfg of
          ASMJS      -> map ParamStm params
          JavaScript -> []
    foldr (.+.) (pure [])
      [ str "(function(", str params', str ")"
      , fromJS (Block (argdecls ++ map DeclStm locals ++ body))
      , str ")"
      ]

str :: JSString -> Printer
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
