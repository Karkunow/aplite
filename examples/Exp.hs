{-# LANGUAGE QuasiQuotes #-}

module Exp where



import Language.C.Quote.C

import Language.Embedded.Imperative



data Exp a where
  Var :: VarId -> Exp a
  Lit :: (Eq a, Show a) => a -> Exp a
  Add :: Exp Int -> Exp Int -> Exp Int

type instance VarPred Exp = Eq :/\: Show

instance EvalExp Exp
  where
    litExp = Lit
    evalExp (Lit a) = a
    evalExp (Add a b) = evalExp a + evalExp b

instance CompExp Exp
  where
    varExp = Var

    compExp (Lit l)   = return [cexp| $id:s |]
      where
        s = show l
    compExp (Var v)   = return [cexp| $id:v |]
    compExp (Add a b) = do
        a' <- compExp a
        b' <- compExp b
        return [cexp| $a' + $b' |]
