{-# LANGUAGE OverloadedStrings, TypeFamilies, FlexibleInstances #-}
-- | Exporting functions from Aplite to Haskell.
module Language.JS.Export
  ( Export (..)
  , ReturnValue (..)
  , Param
  , Aplite
  , ApliteCMD
  ) where
import Language.Embedded.Imperative
import Language.Embedded.Imperative.CMD (Arr (..), IArr (..), ReturnValue (..))
import Language.JS.CompExp
import Language.JS.Expression hiding (Fun)
import Language.JS.Syntax
import Language.JS.Monad
import Haste (toJSString)
import qualified Haste.JSString as S

-- | The Aplite monad. All Aplite programs execute in this monad.
type Aplite a = Program ApliteCMD a

type ApliteCMD = RefCMD CExp :+: ControlCMD CExp :+: ArrCMD CExp

{-
-- ReturnValue instances for arrays
instance JSType e => ReturnValue (Arr i e) where
  returnStmt (ArrComp a) = Just . pure . Ret . typed t . Id . MkId $ a
    where t = Arr (jsType (undefined :: CExp e))

instance JSType e => ReturnValue (IArr i e) where
  returnStmt (IArrComp a) = Just . pure . Ret . typed t . Id . MkId $ a
    where t = Arr (jsType (undefined :: CExp e))
-}

-- Export class + instances: inject arguments into lambda, and marshal them
-- into JS
class ReturnValue (Res f) => Export f where
  type Res f
  export :: Int -> [Param] -> f -> ([Param], Aplite ())

instance (JSType a, Export b) => Export (CExp a -> b) where
  type Res (CExp a -> b) = Res b
  export n as f = export (succ n) (param t (MkId n') : as) (f argexp)
    where n' = S.append "arg" (toJSString n)
          t = jsType (undefined :: CExp a)
          argexp = varExp n'

instance (JSType i, JSType e, Export b) => Export (Arr i e -> b) where
  type Res (Arr i e -> b) = Res b
  export n as f = export (succ n) (param t (MkId name) : as) (f argexp)
    where t = Arr (jsType (undefined :: CExp e))
          name = S.append "arr_arg" (toJSString n)
          argexp = ArrComp name

instance ReturnValue a => Export (Program ApliteCMD a) where
  type Res (Program ApliteCMD a) = a
  export _ as body = (reverse as, body >>= return_)
