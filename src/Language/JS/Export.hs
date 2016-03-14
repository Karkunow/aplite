{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
-- | Exporting functions from Aplite to Haskell.
module Language.JS.Export where
import Language.Embedded.Imperative
import Language.Embedded.Imperative.CMD (Arr (..), IArr (..))
import Language.JS.CompExp
import Language.JS.Expression hiding (Fun)
import Language.JS.Syntax
import Language.JS.Monad
import Haste (toJSString)
import qualified Haste.JSString as S

data Fun a = Fun
  { cgStartId    :: Int
  , expFunParams :: [Param]
  , expFunBody   :: Program ApliteCMD a
  }

type ApliteCMD = RefCMD CExp :+: ControlCMD CExp :+: ArrCMD CExp

type family RetVal a where
  RetVal (CExp a) = IO a
  RetVal a        = IO a

-- ReturnValue instances for arrays
instance JSType e => ReturnValue (Arr i e) where
  returnStmt (ArrComp a) = Just . pure . Ret . typed t . Id . MkId $ a
    where t = Arr (jsType (undefined :: CExp e))

instance JSType e => ReturnValue (IArr i e) where
  returnStmt (IArrComp a) = Just . pure . Ret . typed t . Id . MkId $ a
    where t = Arr (jsType (undefined :: CExp e))

-- Export class + instances: inject arguments into lambda, and marshal them
-- into JS
class ReturnValue (Res f) => Export f where
  type Res f
  mkFun :: Int -> [Param] -> f -> Fun (Res f)

instance (JSType a, Export b) => Export (CExp a -> b) where
  type Res (CExp a -> b) = Res b
  mkFun n as f = mkFun (succ n) (param t (MkId n') : as) (f argexp)
    where n' = S.cons 'n' (toJSString n)
          t = jsType (undefined :: CExp a)
          argexp = varExp n'

instance (JSType i, JSType e, Export b) => Export (Arr i e -> b) where
  type Res (Arr i e -> b) = Res b
  mkFun n as f = mkFun (succ n) (param t (MkId name) : as) (f argexp)
    where t = Arr (jsType (undefined :: CExp e))
          name = S.cons 'a' (toJSString n)
          argexp = ArrComp name

instance ReturnValue a => Export (Program ApliteCMD a) where
  type Res (Program ApliteCMD a) = a
  mkFun n as body = Fun
      { cgStartId    = n
      , expFunParams = reverse as
      , expFunBody   = body
      }
