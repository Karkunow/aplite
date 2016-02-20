-- | Interface for evaluation and compilation of pure expressions
module Language.Embedded.Expression
  ( VarPred
  , EvalExp(..)
  )
  where

import Data.Proxy
import Data.Constraint

-- | Constraint on the types of variables in a given expression language
type family VarPred (exp :: * -> *) :: * -> Constraint

-- | General interface for evaluating expressions
class EvalExp exp
  where
    -- | Literal expressions
    litExp  :: VarPred exp a => a -> exp a

    -- | Evaluation of (closed) expressions
    evalExp :: exp a -> a
