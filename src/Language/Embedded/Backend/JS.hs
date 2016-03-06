-- | JavaScript backend for imperative-edsl.
module Language.Embedded.Backend.JS where
import Control.Monad.Operational.Higher
import Language.Embedded.Imperative.Backend.JS
import Language.JS.Monad
import Language.JS.Print
import Language.JS.Expression hiding (Fun)
import Language.JS.CompExp
import Language.JS.Export
import Data.Proxy
import Haste (JSString)

import Data.Bits
import Language.JS.Syntax (Func (..))
import Language.Embedded.Imperative

compile :: Export a => CodeTuning -> a -> JSString
compile ct f =
    wrapped ct $ f' {funParams = params}
  where
    Fun startid params prog = mkFun 0 [] f
    f' = generate startid prog

generate :: (ReturnValue a, Interp instr JSGen, HFunctor instr)
        => Int -> Program instr a -> Func
generate startid = runJSGen startid . interpret
