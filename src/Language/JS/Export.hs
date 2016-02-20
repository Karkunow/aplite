{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
-- | Exporting functions from Aplite to Haskell.
module Language.JS.Export where
import Language.Embedded.Imperative
import Language.JS.CompExp
import Language.JS.Expression hiding (Fun)
import Language.JS.Syntax
import Language.JS.Monad

data Fun instr a = Fun
  { cgStartId    :: Integer
  , expFunParams :: [Param]
  , expFunBody   :: Program instr a
  }

class ReturnValue (Res f) => Export f where
  type Res f
  type Instr f :: (* -> *) -> * -> *
  mkFun :: Integer -> [Typed Id] -> f -> Fun (Instr f) (Res f)

instance (JSType a, Export b) => Export (CExp a -> b) where
  type Res (CExp a -> b) = Res b
  type Instr (CExp a -> b) = Instr b
  mkFun n as f = mkFun (succ n) (Typed t (MkId n) : as) (f argexp)
    where t = jsType (undefined :: CExp a)
          argexp = varExp (MkId n)

instance ReturnValue a => Export (Program instr a) where
  type Res (Program instr a) = a
  type Instr (Program instr a) = instr
  mkFun n as body = Fun
      { cgStartId    = n
      , expFunParams = map unUnsigned $ reverse as
      , expFunBody   = body
      }

-- | Unsigned is not a valid argument type in ASM.js, so we change the
--   annotation on all unsigned arguments to signed, which is equivalent as
--   the value will be converted back into unsigned whenever it's used.
unUnsigned :: Typed a -> Typed a
unUnsigned (Typed Unsigned x) = Typed Signed x
unUnsigned x                  = x
