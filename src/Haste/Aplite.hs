{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, BangPatterns,
             UndecidableInstances #-}
module Haste.Aplite
  ( -- * Creating Aplite functions
    Aplite, ApliteExport, ApliteSig, ApliteCMD
  , aplite, apliteWith, compile
    -- * Tuning Aplite code to the browser environment
  , CodeTuning (..), CodeStyle (..), CodeHeader (..), defaultTuning, asmjsTuning
    -- * Aplite language stuff
  , CExp, ArrView, Index, Length
  , Bits (..), shiftRL
  , true, false, not_
  , (#&&), (#||), (#==), (#!=), (#<), (#>), (#<=), (#>=), (#!)
  , fmod, sqrt_, quot_, round_, floor_, ceiling_, i2n, i2b, f2n, (#%), share
-- not supported yet!  , cond, (?), (#!)
  , module Language.Embedded.Imperative
  , module Data.Int
  , module Data.Word
  , module Data.Array.IO
  , module Data.Array.Unboxed
  ) where
import Control.Monad.Operational.Higher
import Language.JS.Print
import Language.JS.Export
import Language.Embedded.Backend.JS
import Haste.Foreign
import Haste.Prim (veryUnsafePerformIO)
import Haste (JSString)

import Language.JS.Expression
import Language.Embedded.Imperative
import Data.Bits
import Data.Int
import Data.Word
import Data.Array.IO
import Data.Array.Unboxed

type Index = Word32
type Length = Index

-- | The Aplite monad. All Aplite programs execute in this monad.
type Aplite a = Program ApliteCMD a

-- | A Haskell type which has a corresponding Aplite type. A Haskell type has
--   a corresponding Aplite type if it is exportable using "Haste.Foreign",
--   if its parameters return value are all representable in Aplite, and if
--   all arguments are safe in the context of the return type.
--   If the return type is @IO a@, then any representable argument is safe.
--   If the return type is a pure value, then only immutable arguments are
--   considered safe.
type ApliteExport a =
  ( FFI (FFISig a)
  , Export (ApliteSig a)
  , a ~ NoIO (FFISig a) (Purity a)
  , UnIO (FFISig a) (Purity a)
  )

-- | Explicitly share an Aplite expression.
share :: (JSType a', a ~ CExp a') => a -> Aplite a
share x = initRef x >>= unsafeFreezeRef

-- | Compile an aplite function using the default code tuning.
aplite :: forall a. ApliteExport a => ApliteSig a -> a
aplite = apliteWith defaultTuning

-- | Compile an Aplite function and lift it into Haskell proper.
--   Aplite functions with no observable side effects may be imported as pure
--   Haskell functions:
--
--     apAdd :: Int32 -> Int32 -> Int32
--     apAdd = aplite defaultTuning $ \a b -> return (a+b)
--
--   They may also be imported as functions in the IO monad:
--
--     apAddIO :: Int32 -> Int32 -> IO Int32
--     apAddIO = aplite defaultTuning $ \a b -> return (a+b)
--
--   Functions which may perform observable side effects or have mutable
--   arguments may only be imported in the IO monad:
--
--     memset :: IOUArray Int32 Int32 -> Int32 -> Int32 -> IO Int32
--     memset = aplite defaultTuning $ \arr len elem ->
--       for (0, 1, Excl len) $ \i -> do
--         setArr i elem arr
--
--   Note that Aplite functions are monomorphic, as @aplite@ compiles them
--   to highly specialized, low level JavaScript.
apliteWith :: forall a. ApliteExport a => CodeTuning -> ApliteSig a -> a
apliteWith t !prog = unIO (undefined :: Purity a) $! prog'
  where
    prog' :: FFISig a
    prog' = ffi $! compile t prog

-- | The FFI signature corresponding to the given type signature. Always in the
--   IO monad due to how Haste.Foreign works.
type family FFISig a where
  FFISig (a -> b) = (a -> FFISig b)
  FFISig (IO a)   = IO a
  FFISig a        = IO a

-- | The Aplite level signature corresponding to the given Haskell level
--   signature. Unsafe arguments, such as mutable arrays, may only appear in
--   @Impure@ aplite signatures, which ensures that side effecting code may
--   not be unsafely imported.
type family ApliteSig a where
  ApliteSig (a -> b) = (ApliteArg a (Purity b) -> ApliteSig b)
  ApliteSig a        = ApliteResult a

-- | Denotes a pure Aplite signature: the function may not perform side effects
--   that are observable from Haskell.
data Pure

-- | Denotes an import Aplite signature: the function may perform arbitrary
--   side effects.
data Impure

-- | Is the given value impure, (an IO computation), or pure (any other value)?
type family Purity a where
  Purity (a -> b) = Purity b
  Purity (IO a)   = Impure
  Purity a        = Pure

-- | Valid return types for imported Aplite functions.
type family ApliteResult a where
  ApliteResult (IO (IOUArray i e)) = Aplite (Arr i e)
  ApliteResult (IO (UArray i e))   = Aplite (IArr i e)
  ApliteResult (IO ())             = Aplite ()
  ApliteResult (IO a)              = Aplite (CExp a)
  ApliteResult (IOUArray i e)      = Aplite (Arr i e)
  ApliteResult (UArray i e)        = Aplite (IArr i e)
  ApliteResult a                   = Aplite (CExp a)

-- | All arguments that can be passed to Aplite functions.
--   The @p@ parameter denotes the purity of an argument; if @Pure@, unsafe
--   arguments, such as mutable arrays, will not unify.
type family ApliteArg a p where
  ApliteArg Double         p      = CExp Double
  ApliteArg Int            p      = CExp Int32
  ApliteArg Int32          p      = CExp Int32
  ApliteArg Word           p      = CExp Word32
  ApliteArg Word32         p      = CExp Word32
  ApliteArg Bool           p      = CExp Bool
  ApliteArg (UArray i e)   p      = IArr i e
  ApliteArg (IOUArray i e) Impure = Arr i e

-- | If @p@ is @Pure@, converts the given function of the form
--   @a -> ... -> IO b@ to a function @a -> ... -> b@.
--   If @p@ is @Impure@, does nothing.
class UnIO a p where
  type NoIO a p
  unIO :: p -> a -> NoIO a p

instance UnIO (IO a) Pure where
  type NoIO (IO a) Pure = a
  unIO _ = veryUnsafePerformIO

instance UnIO (IO a) Impure where
  type NoIO (IO a) Impure = IO a
  unIO _ = id

instance UnIO b Pure => UnIO (a -> b) Pure where
  type NoIO (a -> b) Pure = a -> NoIO b Pure
  unIO p f = \x -> unIO p (f x)

instance UnIO (a -> b) Impure where
  type NoIO (a -> b) Impure = a -> b
  unIO _ = id

-- | The Haste-internal name of the ArrayBuffer view for the given type.
class ArrView a where
  arrView :: a -> JSString

instance ArrView Double where arrView _ = "f64"
instance ArrView Int    where arrView _ = "i32"
instance ArrView Int32  where arrView _ = "i32"
instance ArrView Word   where arrView _ = "w32"
instance ArrView Word32 where arrView _ = "w32"

iouarrToAny :: JSString -> Opaque (IOUArray i e) -> IO JSAny
iouarrToAny = ffi "(function(v,a){return a.d['v'][v];})"

anyToIOUArr :: JSAny -> IO (Opaque (IOUArray i e))
anyToIOUArr =
  ffi "(function(a){return new T4(0,0,a['length']-1,a['length'],wrapByteArr(a['buffer']));})"

uarrToAny :: JSString -> Opaque (UArray i e) -> IO JSAny
uarrToAny = ffi "(function(v,a){return a.d['v'][v];})"

anyToUArr :: JSAny -> IO (Opaque (UArray i e))
anyToUArr =
  ffi "(function(a){return new T4(0,0,a['length']-1,a['length'],wrapByteArr(a['buffer']));})"

instance forall i e. ArrView e => FromAny (IOUArray i e) where
  fromAny x = fromOpaque <$> anyToIOUArr x

instance forall i e. ArrView e => ToAny (IOUArray i e) where
  toAny x =
    veryUnsafePerformIO $ iouarrToAny (arrView (undefined :: e)) (toOpaque x)

instance forall i e. ArrView e => FromAny (UArray i e) where
  fromAny x = fromOpaque <$> anyToUArr x

instance forall i e. ArrView e => ToAny (UArray i e) where
  toAny x =
    veryUnsafePerformIO $ uarrToAny (arrView (undefined :: e)) (toOpaque x)
