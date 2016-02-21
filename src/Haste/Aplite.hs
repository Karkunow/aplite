module Haste.Aplite
  ( -- * Creating Aplite functions
    ApliteExport, ApliteCMD, aplite, compile
    -- * Tuning Aplite code to the browser environment
  , CodeTuning (..), CodeStyle (..), CodeHeader (..), defaultTuning
    -- * Aplite language stuff
  , CExp
  , Bits (..)
  , true, false, not_, (#&&), (#||), (#==), (#!=), (#<), (#>), (#<=), (#>=)
-- not supported yet!  , cond, (?), (#!)
  , module Language.Embedded.Imperative
  , module Data.Int
  , module Data.Word
  ) where
import Control.Monad.Operational.Higher
import Language.JS.Print
import Language.JS.Export
import Language.Embedded.Backend.JS
import Haste.Prim
import Haste.Foreign
import Data.Proxy

import Language.JS.Expression
import Language.Embedded.Imperative
import Data.Bits
import Data.Int
import Data.Word

type ApliteExport a =
  ( InterpCMD a ~ Program ApliteCMD (Res a)
  , Export a
  , FFI (ExportSig a))

aplite :: ApliteExport a => CodeTuning -> a -> ExportSig a
aplite t = ffi . toJSStr . compile t

type family InterpCMD f where
  InterpCMD (a -> b) = InterpCMD b
  InterpCMD a        = a
