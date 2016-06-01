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
compile ct = compileFromAST ct . compileToAST

compileToAST :: Export a => a -> Func
compileToAST f = f' {funParams = params}
  where
    (params, prog) = export 0 [] f
    f' = generate prog

compileFromAST :: CodeTuning -> Func -> JSString
compileFromAST = wrapped

generate :: Aplite () -> Func
generate = runJSGen 0 . interpret
