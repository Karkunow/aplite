-- | JavaScript backend for imperative-edsl.
module Language.Embedded.Backend.JS where
import Control.Monad.Operational.Higher
import Language.Embedded.Imperative.Backend.JS
import Language.JS.Monad
import Language.JS.Print
import Language.JS.Expression 
import Language.JS.CompExp

import Data.Bits
import Language.JS.Syntax
import Language.Embedded.Imperative

-- TODO: this should be a config, specifying how to select tuning
compile :: (Interp instr JSGen, HFunctor instr)
        => CodeTuning -> Program instr a -> String
compile cfg = printJS cfg . runJSGen_  . interpret

type CMD = RefCMD CExp :+: ControlCMD CExp

def = defaultTuning
asm = defaultTuning {codeStyle = ASMJS}

-- TODO: hur skapa funktioner?
prog :: Program CMD ()
prog = do
  done <- initRef false
  sum <- initRef (0 :: CExp Int32)
  while (not_ <$> getRef done) $ do
    setRef done true
    x <- getRef sum
    setRef sum (x+x+x+x+x `xor` x)
