{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
-- | Exporting functions from Aplite to Haskell.
module Language.JS.Export where
import Language.Embedded.Imperative
import Language.JS.CompExp
import Language.JS.Expression hiding (Fun)
import Language.JS.Syntax
import Language.JS.Monad

data Fun a = Fun
  { cgStartId    :: Integer
  , expFunParams :: [Param]
  , expFunBody   :: Program ApliteCMD a
  }

type ApliteCMD = RefCMD CExp :+: ControlCMD CExp :+: ArrCMD CExp

type family RetVal a where
  RetVal (CExp a) = IO a
  RetVal a        = IO a

class ReturnValue (Res f) => Export f where
  type Res f
  type ExportSig f
  mkFun :: Integer -> [Param] -> f -> Fun (Res f)

instance (JSType a, Export b) => Export (CExp a -> b) where
  type Res (CExp a -> b) = Res b
  type ExportSig (CExp a -> b) = a -> ExportSig b
  mkFun n as f = mkFun (succ n) (param t (MkId n) : as) (f argexp)
    where t = jsType (undefined :: CExp a)
          argexp = varExp (MkId n)

instance ReturnValue a => Export (Program ApliteCMD a) where
  type Res (Program ApliteCMD a) = a
  type ExportSig (Program ApliteCMD a) = RetVal a
  mkFun n as body = Fun
      { cgStartId    = n
      , expFunParams = reverse as
      , expFunBody   = body
      }
