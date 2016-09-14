{-# LANGUAGE CPP, OverloadedStrings #-}
module Language.JS.Print where
import Language.JS.Syntax hiding (typed)
import Control.Monad
import Control.Monad.Cont
import Data.IORef

import qualified Haste.JSString as S
import Haste (toJSString)
import Haste.Foreign
import Haste.Prim
import Data.IORef

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
  { codeStyle    :: !CodeStyle
  , headerDecl   :: !CodeHeader
  , explicitHeap :: !(Maybe HeapSize)
  }

defaultTuning :: CodeTuning
defaultTuning = CodeTuning
  { codeStyle    = JavaScript
  , headerDecl   = StrictHeader
  , explicitHeap = Nothing
  }

asmjsTuning :: CodeTuning
asmjsTuning = CodeTuning
  { codeStyle    = ASMJS
  , explicitHeap = Just 0x10000
  , headerDecl   = ASMHeader
  }

-- | JavaScript @"use X";@ declaration for the given code header.
headerDeclStr :: CodeHeader -> JSString
headerDeclStr StrictHeader = "\"use strict\";"
headerDeclStr ASMHeader    = "\"use asm\";"
headerDeclStr NoHeader     = ""

-- | The function's wrapper: if present, the Aplite function will be
--   wrapped in another function, which is responsible for performing any
--   needed setup, calling the Aplite function, performing cleanup, and
--   returning the return value of the Aplite function.
wrapped :: CodeTuning -> Func -> JSString
wrapped t f@(Func ps ls b) =
  case explicitHeap t of
    Just _ ->
      S.concat
        [ "(function(){\n"
        , "var f = ", thecode, ";\n"
        , "return (function("
        , S.intercalate "," $ map (unId . paramName) ps
        , "){\n"
        , wrapper
        , "\n});\n"
        , "})()"
        ]
    _ ->
      thecode
  where
    thecode = printJS t f

    -- generate a pointer name if pointer, otherwise use original name
    arr2ptr (Arr _) n = S.append n "_ptr"
    arr2ptr _       n = n

    -- generate names for all pointer args; keep the non-pointer ones
    -- unmodified
    ptrs = [arr2ptr t n | Param t (MkId n) <- ps]

    -- set up the heap views
    preamble =
      [ "var hn = new Uint32Array(f.heap);\n"
      , "var hf = new Float64Array(f.heap);"
      ]

    -- declare an array in the ASM.js heap and copy the source array into it
    mkHeapArr (Param (Arr pt) (MkId arr)) ptr = S.concat
      [ "var ", ptr, "=f.malloc(",toJSString (sizeof pt),",",arr,".length);\n"
      , heap, ".set(", arr, ",", ptr, ");"
      ]
      where heap = if sizeof pt == 4 then "hn" else "hf"

    -- copy an array from the ASM.js heap into the array whence the data
    -- originally came
    copyOut (Param (Arr pt) (MkId arr)) ptr = S.concat
      [arr, ".set(", heap, ".subarray(", ptr, ",", ptr, "+", arr, ".length));"]
      where heap = if sizeof pt == 4 then "hn" else "hf"

    -- all array arguments and their corresponding heap pointers
    arrArgs = [(p, ptr) | (p@(Param (Arr _) _), ptr) <- zip ps ptrs]

    -- the actual wrapper code
    wrapper = S.intercalate "\n"
      [ S.concat preamble
      , S.intercalate "\n" $ map (uncurry mkHeapArr) arrArgs
      , S.concat ["var result = f(", S.intercalate "," ptrs, ");"]
      , S.intercalate "\n" $ map (uncurry copyOut) arrArgs
      , "f.malloc.next_addr = 0;"
      , "return result;"
      ]

data PrintEnv = PrintEnv
  { indent  :: !JSString
  , tuning  :: !CodeTuning
  }

type PrintM = ContT JSString IO
type Printer = PrintM ()

{-# NOINLINE envref #-}
envref :: IORef PrintEnv
envref = veryUnsafePerformIO $ newIORef undefined

{-# INLINE ask #-}
ask :: PrintM PrintEnv
ask = lift $ readIORef envref

{-# INLINE local #-}
local :: (PrintEnv -> PrintEnv) -> PrintM a -> PrintM a
local f m = do
  env <- ask
  lift $ writeIORef envref $! (f env)
  x <- m
  lift $ writeIORef envref $! env
  return x

runPrinter :: CodeTuning -> Printer -> JSString
runPrinter t m = veryUnsafePerformIO $ do
  newBuilder_
  writeIORef envref $! PrintEnv "" t
  runContT m (const finalize_)

#ifdef __HASTE__
type Builder = JSAny

newBuilder_ :: IO ()
newBuilder_ = ffi "(function(){window.__b = [];})"

push_ :: JSString -> IO ()
push_ = ffi "(function(s){window.__b.push(s);})"

finalize_ :: IO JSString
finalize_ = ffi "(function(){var r = window.__b.join(''); delete window.__b; return r;})"

#else

{-# NOINLINE builderref #-}
builderref :: IORef Builder
builderref = veryUnsafePerformIO $ newIORef []

type Builder = [JSString]

push_ :: JSString -> IO ()
push_ s = atomicModifyIORef' builderref (\ss -> (s:ss, ()))

finalize_ :: IO JSString
finalize_ = S.concat . reverse <$> readIORef builderref

newBuilder_ :: IO ()
newBuilder_ = writeIORef builderref []
#endif

{-# INLINE push #-}
push :: JSString -> Printer
push s = lift $ push_ s

printJS :: PrintJS a => CodeTuning -> a -> JSString
printJS t = wrap t . runPrinter t . fromJS

-- | Add pre- and post scaffolding to a string of JS based on the current
--   code tuning.
--   TODO: add HOFs to FFI?
wrap :: CodeTuning -> JSString -> JSString
wrap t prog
  | codeStyle t == ASMJS =
    S.concat
      [ "(function(){\n"
      , "var heap = new ArrayBuffer(", heapsize, ");\n"
      , "var malloc = ", mallocASM, ";\n"
      , "var f = ("
      , asmModule t memcpyASM prog
      , ")(window, "
      , ffifuns (codeStyle t)
      , ", heap).f;\n"
      , "f.heap = heap;\n"
      , "f.malloc = malloc;\n"
      , "return f;\n"
      , "})()"
      ]
  | otherwise =
    S.concat
      [ "("
      , jsModule t prog
      , ")(window, ", ffifuns (codeStyle t), ")"
      ]
  where
    heapsize = maybe "0" toJSString (explicitHeap t)
    ffifuns ASMJS      = S.concat [ "{ malloc: malloc"
                                  , "}"]
    ffifuns JavaScript = S.concat [ "{ malloc:", mallocJS
                                  , ", memcpy:", memcpyJS
                                  , "}"]
    -- malloc takes an element size and a # of elements to allocate
    mallocASM = "function malloc(sz, len) {\
        if(typeof malloc.next_addr === 'undefined') {\
          malloc.next_addr = 0;\
        }\
        var ptr = malloc.next_addr/sz;\
        var bytes = len*sz;\
        bytes += (8 - (bytes % 8)) % 8;\
        malloc.next_addr += bytes;\
        return ptr;\
      }"
    mallocJS = "function mallocJS(sz, len) {\
        var buf = new ArrayBuffer(sz*len);\
        if(len == 8) {\
          return new Float64Array(buf);\
        } else {\
          return new Int32Array(buf);\
        }\
      }"
    memcpyJS = "function(from, to, sz, elems) {\
        if(elems == from.length) {\
          to.set(from);\
        } else {\
          for(var i = 0; i < elems; ++i) {\
            to[i] = from[i];\
          }\
        }\
      }"
    memcpyASM = "function memcpy(from, to, sz, elems) {\
        from = from|0;\
        to = to|0;\
        sz = sz|0;\
        elems = elems|0;\
        var i = 0;\
        if((sz|0) == (8|0)) {\
          for(i = 0; (i|0) < (elems|0); i = (i+1)|0) {\
              hf[(((to+i)|0)<<3)>>3] = hf[(((from+i)|0)<<3)>>3];\
          }\
        } else {\
          for(i = 0; (i|0) < (elems|0); i = (i+1)|0) {\
              hn[(((to+i)|0)<<2)>>2] = hn[(((from+i)|0)<<2)>>2];\
          }\
        }\
      }"

-- | JSString representation of an ASM.js module.
asmModule :: CodeTuning -> JSString -> JSString -> JSString
asmModule t memcpy prog = S.intercalate "\n"
  [ "function(stdlib, ffi, heap){"
  , headerDeclStr (headerDecl t)
  , "var hn = new stdlib.Int32Array(heap);"
  , "var hf = new stdlib.Float64Array(heap);"
  , stdlibImports
  , "var malloc = ffi.malloc;"
  , memcpy
  , prog
  , "return ({f:f});"
  , "}"
  ]

-- | JSString representation of a plain JS module.
jsModule :: CodeTuning -> JSString -> JSString
jsModule t prog = S.intercalate "\n"
  [ "function(stdlib, ffi){"
  , headerDeclStr (headerDecl t)
  , stdlibImports
  , "var malloc = ffi.malloc;"
  , "var memcpy = ffi.memcpy;"
  , prog
  , "return f;"
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

instance PrintJS ArrId where
  fromJS = push

instance PrintJS Param where
  fromJS (Param t n) = do
    fromJS n
    push " = "
    typed t (fromJS n)

instance PrintJS Id where
  fromJS (MkId n)     = push n

instance PrintJS (Typed Id) where
  needsParen (Typed _ _) = codeStyle . tuning <$> ask >>= \cs -> case cs of
    ASMJS      -> pure True
    JavaScript -> pure False
  fromJS (Typed _ x) = fromJS x

instance PrintJS (Typed ArrId) where
  needsParen (Typed _ _) = pure False
  fromJS (Typed _ x)     = fromJS x

instance PrintJS (Typed Exp) where
  needsParen (Typed _ x) = codeStyle . tuning <$> ask >>= \cs -> case cs of
    ASMJS      -> pure True
    JavaScript -> needsParen x

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
  fromJS (Decl t n) = do
      codeStyle . tuning <$> ask >>= \cs -> case cs of
        ASMJS      -> push "var " >> fromJS n >> push " = " >> push (zeroFor t)
        JavaScript -> push "var " >> fromJS n

instance PrintJS BinOp where
  fromJS op = push " " >> push (toJSString op) >> push " "

instance PrintJS Exp where
  needsParen (Id _)  = pure False
  needsParen (Lit _) = pure False
  needsParen _       = pure True

  fromJS (Id n)       = fromJS n
  fromJS (Op op a b)  = codeStyle . tuning <$> ask >>= \cs -> case cs of
    ASMJS      -> parenIfNecessary a >> fromJS op >> parenIfNecessary b
    JavaScript -> overflowIfNecessary op a b
  fromJS (Lit x)
    | isIntegral x    = push (toJSString (truncate x :: Int))
    | otherwise       = push (toJSString x)
  fromJS (Neg x)      = push "-(" >> fromJS x >> push ")"
  fromJS (Not x)      = push "!(" >> fromJS x >> push ")"
  fromJS (Cast t x)   = codeStyle . tuning <$> ask >>= \cs -> case cs of
    ASMJS      -> genCastASM t x
    JavaScript -> genCastJS t x
  fromJS (Cond _ _ _) = error "TODO: ternary operator not supported in asm.js!"
  fromJS (Call f as)  = do
    push (toJSString f)
    push "("
    sepBy "," (map fromJS as)
    push ")"
  fromJS (Index t arr ix) = do
    codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS      -> asmIx t arr ix
      JavaScript -> push arr >> push "[" >> fromJS ix >> push "]"

overflowIfNecessary :: BinOp -> Typed Exp -> Typed Exp -> Printer
overflowIfNecessary op a@(Typed t _) b
  | t /= Double && overflowOp op = typed t (paren noOverflow)
  | otherwise                    = noOverflow
  where
    noOverflow = parenIfNecessary a >> fromJS op >> parenIfNecessary b

sepBy :: JSString -> [Printer] -> Printer
sepBy _ []     = return ()
sepBy s (p:ps) = p >> when (not $ null ps) (push s) >> sepBy s ps

asmIx :: Type -> ArrId -> Typed Exp -> Printer
asmIx t arr ix = do
    push (heap t)
    push "[("
    push arr
    push "+("
    fromJS ix
    push ")<<"
    pushShift
    push ")>>"
    pushShift
    push "]"
  where
    pushShift = case t of
      Double -> push "3"
      _      -> push "2"

-- | Get the appropriate heap view for the given type.
heap :: Type -> JSString
heap Double = "hf"
heap _      = "hn"

genCastASM :: Type -> Typed Exp -> Printer
genCastASM Signed (Typed Double x)   = push "(~~(" >> fromJS x >> push "))"
genCastASM Unsigned (Typed Double x) = push "((~~(" >> fromJS x >> push "))>>>0)"
genCastASM _ x                       = fromJS x

genCastJS :: Type -> Typed Exp -> Printer
genCastJS Signed (Typed Double x)   = push "((" >> fromJS x >> push ")|0)"
genCastJS Unsigned (Typed Double x) = push "((" >> fromJS x >> push ")>>>0)"
genCastJS _ x                       = fromJS x

isIntegral :: Double -> Bool
isIntegral x = x == fromIntegral (truncate x :: Int)

instance PrintJS Stmt where
  fromJS (a := b) =
    fromJS a >> push " = " >> fromJS b
  fromJS (DeclStm d) =
    fromJS d
  fromJS (ParamStm p) =
    fromJS p
  fromJS (ExpStm (Typed _ e)) =
    fromJS e
  fromJS (Inc x) = do
    codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS -> do
        fromJS x
        push " = "
        typed (typeOf x) (paren $ fromJS x >> push "+1")
      JavaScript -> do
        push "++"
        fromJS x
  fromJS (Dec x) = do
    codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS -> do
        fromJS x
        push " = "
        typed (typeOf x) (paren $ fromJS x >> push "-1")
      JavaScript -> do
        push "--"
        fromJS x
  fromJS (Ret x) =
    push "return " >> fromJS x
  fromJS (Block ss) = do
    ind <- indent <$> ask
    let ind' = S.concat ["  ", ind]
        lineSep = S.append ";\n" ind'
    push "{\n"
    push ind'
    local (\env -> env {indent = ind'}) $ sepBy lineSep (map fromJS ss)
    push ";\n"
    push ind
    push "}"
  fromJS (If e a mb) = do
    push "if(" >> fromJS e >> push ")"
    fromJS a
    case mb of
      Just b -> push "else" >> fromJS b
      _      -> return ()
  fromJS (Forever s) =
    push "while(1)" >> fromJS s
  fromJS (For i cond step body) = do
    push "for(" >> fromJS i >> push ";"
    fromJS cond >> push ";"
    fromJS step >> push ")"
    fromJS body
  fromJS Break =
    push "break"
  fromJS (Assert _ _) =
    error "TODO: assert"
  fromJS (Write arr ix x) = do
    codeStyle . tuning <$> ask >>= \cs -> case cs of
      ASMJS ->
        asmIx t arr ix >> push " = " >> fromJS x
      JavaScript ->
        push arr >> push "[" >> fromJS ix >> push "] = " >> fromJS x
    where t = typeOf x

instance PrintJS Func where
  fromJS (Func params locals body) = do
    cfg <- tuning <$> ask
    let argdecls = case codeStyle cfg of
          ASMJS      -> map ParamStm params
          JavaScript -> []
    push "function f("
    sepBy "," (map (fromJS . paramName) params)
    push ")"
    fromJS (Block (argdecls ++ map DeclStm locals ++ body))

{-# INLINE typed #-}
typed :: Type -> Printer -> Printer
typed Double m   = push "+" >> m
typed Signed m   = m >> push "|0"
typed Unsigned m = m >> push ">>>0"
typed (Arr _) m  = m >> push "|0"

{-# INLINE paren #-}
paren :: Printer -> Printer
paren m = push "(" >> m >> push ")"
