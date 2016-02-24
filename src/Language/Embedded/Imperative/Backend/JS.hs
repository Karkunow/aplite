{-# LANGUAGE CPP, OverloadedStrings #-}
-- | JavaScript code generation for imperative commands
module Language.Embedded.Imperative.Backend.JS where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad.State
import Data.Proxy

import Control.Monad.Operational.Higher
import Language.Embedded.Imperative.CMD as CMD
import Language.JS.Monad
import Language.JS.Syntax as JS
import Language.JS.CompExp
import qualified Haste.JSString as S

-- | Compile `RefCMD`
compRefCMD :: forall exp prog a. CompJSExp exp
           => RefCMD exp prog a -> JSGen a
compRefCMD cmd@NewRef = do
    t <- compTypePP2 (Proxy :: Proxy exp) cmd
    r <- freshId
    addLocal t r
    return (RefComp (unId r))
compRefCMD (InitRef exp) = do
    t <- compType exp
    r <- freshId
    v <- compExp exp
    addLocal t r
    addStm (r := v)
    return (RefComp (unId r))
compRefCMD cmd@(GetRef (RefComp ref)) = do
    t <- compTypePP2 (Proxy :: Proxy exp) cmd
    v <- declareNewVar t
    addStm (v := typed t (Id (MkId ref)))
    return (varExp v)
compRefCMD (SetRef (RefComp ref) exp) = do
    ex <- compExp exp
    addStm (MkId ref := ex)
compRefCMD (UnsafeFreezeRef (RefComp v)) =
    return (varExp (MkId v))

-- | Compile `ArrCMD`
compArrCMD :: forall exp prog a. (CompJSExp exp, EvalExp exp)
           => ArrCMD exp prog a -> JSGen a
compArrCMD cmd@(NewArr size) = do
    sym <- genSym "a"
    n   <- compExp size
    t   <- compTypePP2 (Proxy :: Proxy exp) cmd
    let name = named sym
    addLocal (Arr t) name
    addStm (name := newArr t n)
    return $ ArrComp sym
compArrCMD cmd@(InitArr as) = do
    sym <- genSym "a"
    t   <- compTypePP2 (Proxy :: Proxy exp) cmd
    as' <- sequence [compExp (litExp a :: exp a') | (a :: a') <- as]
    let n = named sym
    addLocal t n
    addStm (n := newArr t (toTypedExp $ length as))
    forM_ (zip [0 :: Int ..] as') $ \(i, x) ->
      addStm (Write sym (toTypedExp i) x)
    return $ ArrComp sym
compArrCMD (GetArr expi a@(ArrComp arr)) = do
    t <- compTypePP (Proxy :: Proxy exp) a
    n <- declareNewVar t
    i <- compExp expi
    addStm (n := typed t (Index t arr i))
    return (varExp n)
compArrCMD (SetArr expi expv (ArrComp arr)) = do
    v <- compExp expv
    i <- compExp expi
    addStm (Write arr i v)
compArrCMD (CopyArr a@(ArrComp arr1) (ArrComp arr2) expl) = do
    l <- compExp expl
    t <- compTypePP (Proxy :: Proxy exp) a
    let at = Arr t
    -- TODO: this gives length in elements, not in bytes
    addStm (ExpStm $ Typed t $ Call "memcpy" [ typed at (Id $ named arr1)
                                             , typed at (Id $ named arr2)
                                             , toTypedExp (sizeof t)
                                             , l
                                             ])
compArrCMD (UnsafeFreezeArr (ArrComp arr)) = return $ IArrComp arr
compArrCMD (UnsafeThawArr (IArrComp arr))  = return $ ArrComp arr

-- | Compile `ControlCMD`
compControlCMD :: CompJSExp exp => ControlCMD exp JSGen a -> JSGen a
compControlCMD (CMD.If c t f) = do
    cc <- compExp c
    ct <- inBlock_ t
    cf <- inBlock_ f
    case (ct, cf) of
      (Block [], Block []) -> return ()
      (_       , Block []) -> addStm $ JS.If cc ct Nothing
      (Block [], _)        -> addStm $ JS.If (mapTyped Not cc) cf Nothing
      (_       , _)        -> addStm $ JS.If cc ct (Just cf)
compControlCMD (While cont body) = do
    s <- get
    noop <- do
      conte <- cont
      contc <- compExp conte
      return (contc == jsFalse)
    put s
    bodyc <- inBlock_ $ do
        conte <- cont
        contc <- compExp conte
        case untyped contc of
          JS.Lit 1 -> return ()
          Not e    -> addStm $ JS.If e JS.Break Nothing
          _        -> addStm $ JS.If (mapTyped Not contc) JS.Break Nothing
        body
    when (not noop) $ addStm (Forever bodyc)
compControlCMD (CMD.For (lo,step,hi) body) = do
    loe   <- compExp lo
    hie   <- compExp $ borderVal hi
    (i,n) <- declareNew (typeOf loe)
    (_, bodyc) <- inBlock (body (varExp n))
    let incl = borderIncl hi
        step' = sameTypeAs loe (toJSExp step)
        negStep' = sameTypeAs loe (toJSExp (negate step))
        conte
          | incl && (step>=0) = i .<= hie -- [cexp| $id:n<=$hie |]
          | incl && (step<0)  = i .>= hie -- [cexp| $id:n>=$hie |]
          | step >= 0         = i .<  hie -- [cexp| $id:n< $hie |]
          | step < 0          = i .>  hie -- [cexp| $id:n> $hie |]
        stepstm
          | step == 1    = Inc (sameTypeAs loe n) -- [cexp| $id:n++ |]
          | step == (-1) = Dec (sameTypeAs loe n) -- [cexp| $id:n-- |]
          | step == 0    = n := loe -- [cexp| 0 |]
          | step >  0    = n := (i + step') -- [cexp| $id:n = $id:n + $step |]
          | step <  0    = n := (i + negStep') -- [cexp| $id:n = $id:n - $(negate step) |]
    addStm $ JS.For (n := loe) conte stepstm bodyc -- [cstm| for ($id:n=$loe; $conte; $stepe) {$items:bodyc} |]
compControlCMD CMD.Break = addStm JS.Break
compControlCMD (CMD.Assert cond msg) = do
    c <- compExp cond
    addStm $ JS.Assert c (S.pack msg) -- [cstm| assert($c && $msg); |]

{-
compIOMode :: IOMode -> String
compIOMode ReadMode      = "r"
compIOMode WriteMode     = "w"
compIOMode AppendMode    = "a"
compIOMode ReadWriteMode = "r+"

-- | Compile `FileCMD`
compFileCMD :: CompExp exp => FileCMD exp CGen a -> CGen a
compFileCMD (FOpen path mode) = do
    addInclude "<stdio.h>"
    addInclude "<stdlib.h>"
    sym <- gensym "v"
    addLocal [cdecl| typename FILE * $id:sym; |]
    addStm   [cstm| $id:sym = fopen($id:path',$string:mode'); |]
    return $ HandleComp sym
  where
    path' = show path
    mode' = compIOMode mode
compFileCMD (FClose h) = do
    addInclude "<stdio.h>"
    touchVar h
    addStm [cstm| fclose($id:h); |]
compFileCMD (FPrintf h form as) = do
    addInclude "<stdio.h>"
    touchVar h
    let h'     = [cexp| $id:h |]
        form'  = show form
        form'' = [cexp| $id:form' |]
    as' <- fmap ([h',form'']++) $ sequence [compExp a | PrintfArg a <- as]
    addStm [cstm| fprintf($args:as'); |]
compFileCMD cmd@(FGet h) = do
    addInclude "<stdio.h>"
    (v,n) <- freshVar
    touchVar h
    let mkProxy = (\_ -> Proxy) :: FileCMD exp prog (exp a) -> Proxy a
        form    = formatSpecifier (mkProxy cmd)
    addStm [cstm| fscanf($id:h, $string:form, &$id:n); |]
    return v
compFileCMD (FEof h) = do
    addInclude "<stdbool.h>"
    addInclude "<stdio.h>"
    (v,n) <- freshVar
    touchVar h
    addStm [cstm| $id:n = feof($id:h); |]
    return v

compObjectCMD :: CompExp exp => ObjectCMD exp CGen a -> CGen a
compObjectCMD (NewObject t) = do
    sym <- gensym "obj"
    let t' = namedType t
    addLocal [cdecl| $ty:t' * $id:sym; |]
    return $ Object True t sym
compObjectCMD (InitObject fun pnt t args) = do
    sym <- gensym "obj"
    let t' = namedType t
    as  <- mapM mkArg args
    addLocal [cdecl| $ty:t' * $id:sym; |]
    addStm   [cstm|  $id:sym = $id:fun($args:as); |]
    return $ Object pnt t sym
-}

instance CompJSExp exp => Interp (RefCMD exp) JSGen where
  interp = compRefCMD
instance CompJSExp exp => Interp (ControlCMD exp) JSGen where
  interp = compControlCMD
instance (CompJSExp exp, EvalExp exp) => Interp (ArrCMD exp) JSGen where
  interp = compArrCMD

{-
instance CompJSExp exp => Interp (FileCMD exp) JSGen where
  interp = compFileCMD
instance CompJSExp exp => Interp (ObjectCMD exp) JSGen where
  interp = compObjectCMD
-}
