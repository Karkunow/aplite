{-# LANGUAGE ScopedTypeVariables #-}
module Haste.Aplite
  ( -- * Creating Aplite functions
    Aplite, ApliteExport, ApliteCMD, aplite, compile
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
import Haste.Foreign
import Haste.Prim (veryUnsafePerformIO)

import Language.JS.Expression
import Language.Embedded.Imperative
import Data.Bits
import Data.Int
import Data.Word

type Aplite a = Program ApliteCMD (CExp a)

type ApliteExport a =
  ( InterpCMD a ~ Program ApliteCMD (Res a)
  , UnIO (ExportSig a)
  , Export a
  , FFI (ExportSig a))

aplite :: forall a. ApliteExport a => CodeTuning -> a -> NoIO (ExportSig a)
aplite t prog = unIO prog'
  where
    prog' :: ExportSig a
    prog' = ffi (compile t prog)

type family InterpCMD f where
  InterpCMD (a -> b) = InterpCMD b
  InterpCMD a        = a

class UnIO a where
  type NoIO a
  unIO :: a -> NoIO a

instance UnIO (IO a) where
  type NoIO (IO a) = a
  unIO = veryUnsafePerformIO

instance UnIO b => UnIO (a -> b) where
  type NoIO (a -> b) = a -> NoIO b
  unIO f = \x -> unIO (f x)
