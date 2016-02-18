{-# LANGUAGE TypeFamilies #-}
module Language.JS.CompExp
  ( VarPred, EvalExp (..)
  , CompJSExp (..)
  , declareNew, declareNewVar, genVar
  ) where
import Data.Proxy
import Data.Constraint
import Language.JS.Syntax
import Language.JS.Monad
import Language.Embedded.Expression (EvalExp (..), VarPred)

class CompJSExp exp where
  varExp    :: VarPred exp a => Id -> exp a
  
  compExp   :: exp a -> JSGen (Typed Exp)
  
  -- | Extract expression type
  compType :: forall a
           .  VarPred exp a
           => exp a -> JSGen Type
  compType _ = compTypeP (Proxy :: Proxy (exp a))
  {-# INLINE compType #-}

  -- | Extract expression type
  compTypeP :: forall proxy a
            .  VarPred exp a
            => proxy (exp a) -> JSGen Type
  compTypeP _ = compTypePP (Proxy :: Proxy exp) (Proxy :: Proxy a)
  {-# INLINE compTypeP #-}

  -- | Extract expression type
  compTypePP :: forall proxy1 proxy2 a
             .  VarPred exp a
             => proxy1 exp -> proxy2 a -> JSGen Type
  compTypePP _ _ = compTypePP2 (Proxy :: Proxy exp) (Proxy :: Proxy (Proxy a))
  {-# INLINE compTypePP #-}

  -- | Extract expression type
  compTypePP2 :: forall proxy proxy1 proxy2 a
              .  VarPred exp a
              => proxy exp -> proxy1 (proxy2 a) -> JSGen Type
  compTypePP2 _ _ = compType (undefined :: exp a)
  {-# INLINE compTypePP2 #-}

-- | Declare a new local variable and return it.
declareNew :: Type -> JSGen (Typed Exp, Id)
declareNew t = do
  v <- freshId
  addLocal t v Nothing
  return (Typed t (Id v), v)

-- | Declare a new variable.
declareNewVar :: Type -> JSGen Id
declareNewVar = fmap snd . declareNew

-- | Generate a variable from a source language name.
genVar :: Type -> String -> JSGen (Typed Exp)
genVar t = fmap (Typed t . Id) . genIdFor
