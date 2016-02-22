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
  , quot_, round_, i2n, i2b, (#%)
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
  ( FFI (HaskellSig a)
  , Export (ApliteSig a)
  , UnIO (HaskellSig a)
  , a ~ NoIO (HaskellSig a)
  )

aplite :: forall a. ApliteExport a => CodeTuning -> ApliteSig a -> a
aplite t prog = unIO prog'
  where
    prog' :: HaskellSig a
    prog' = ffi (compile t prog)

type family HaskellSig a where
  HaskellSig (a -> b) = (a -> HaskellSig b)
  HaskellSig a        = IO a

type family ApliteSig a where
  ApliteSig (a -> b) = (ApliteArg a -> ApliteSig b)
  ApliteSig a        = Aplite a

type family ApliteArg a where
  ApliteArg Double = CExp Double
  ApliteArg Int    = CExp Int32
  ApliteArg Int32  = CExp Int32
  ApliteArg Word   = CExp Word32
  ApliteArg Word32 = CExp Word32
  ApliteArg Bool   = CExp Bool

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
