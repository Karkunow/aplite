{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Tests for various constructs

module Imperative where



import Data.Int
import Data.Word

import Language.Embedded.Imperative
import Language.Embedded.Backend.C
import Language.Embedded.CExp



type CMD
    =   RefCMD     CExp
    :+: ArrCMD     CExp
    :+: ControlCMD CExp
    :+: PtrCMD
    :+: FileCMD    CExp
    :+: C_CMD      CExp

type Prog = Program CMD



-- | Test primitive types
testTypes :: Prog ()
testTypes = do
    inp :: CExp Int32 <- fget stdin
    a <- unsafeFreezeRef =<< initRef (i2n inp + 0x88               :: CExp Int8)
    b <- unsafeFreezeRef =<< initRef (i2n inp + 0x8888             :: CExp Int16)
    c <- unsafeFreezeRef =<< initRef (i2n inp + 0x88888888         :: CExp Int32)
    d <- unsafeFreezeRef =<< initRef (i2n inp + 0x8888888888888888 :: CExp Int64)
    e <- unsafeFreezeRef =<< initRef (i2n inp + 0xEE               :: CExp Word8)
    f <- unsafeFreezeRef =<< initRef (i2n inp + 0xEEEE             :: CExp Word16)
    g <- unsafeFreezeRef =<< initRef (i2n inp + 0xEEEEEEEE         :: CExp Word32)
    h <- unsafeFreezeRef =<< initRef (i2n inp + 0xEEEEEEEEEEEEEEEE :: CExp Word64)
    i <- unsafeFreezeRef =<< initRef (i2n inp - 9                  :: CExp Float)
    j <- unsafeFreezeRef =<< initRef (i2n inp - 10                 :: CExp Double)
    printf "%d %d %d %ld %u %u %u %lu %.3f %.3f\n" a b c d e f g h i j
    k1 <- unsafeFreezeRef =<< initRef true
    k2 <- unsafeFreezeRef =<< initRef true
    iff ((k1 #&& k2) #|| not_ k1) (printf "true") (printf "false")

testCExp :: Prog ()
testCExp = do
    a :: CExp Int32 <- fget stdin
    let b = a#==10 ? a*3 $ a-5+8
    let c = not_ (a#==10) ? a*3 $ a-5+8
    let d = sin (i2n a) :: CExp Double
    printf "%d %d %.3f" b c d

testRef :: Prog ()
testRef = do
    r1 <- newRef
    r2 <- initRef (3 :: CExp Int32)
    modifyRef r2 (*2)
    setRef r1 =<< getRef r2
    a <- unsafeFreezeRef r1
    b <- unsafeFreezeRef r2
    printf "%d %d\n" a b

testArr1 :: Prog ()
testArr1 = do
    arr1 :: Arr Word32 Int32 <- newArr (10 :: CExp Word32)
    arr2 :: Arr Word32 Int32 <- newArr (10 :: CExp Word32)
    sequence_ [setArr i (i2n i+10) arr1 | i' <- [0..9], let i = fromInteger i']
    copyArr arr2 arr1 10
    sequence_ [getArr i arr2 >>= printf "%d " . (*3) | i' <- [0..9], let i = fromInteger i']
    printf "\n"

testArr2 :: Prog ()
testArr2 = do
    n <- fget stdin
    arr :: Arr Word32 Int32 <- newArr n  -- Array of dynamic length
    sequence_ [setArr (i2n i) i arr | i' <- [0..3], let i = fromInteger i']
    sequence_ [getArr i arr >>= printf "%d " . (*3) | i' <- [0..3], let i = fromInteger i']
    printf "\n"
    return ()

testArr3 :: Prog ()
testArr3 = do
    arr :: Arr Word32 Int32 <- initArr [8,7,6,5]
    sequence_ [getArr i arr >>= printf "%d " . (*3) | i' <- [0..3], let i = fromInteger i']
    printf "\n"
    return ()

testArr4 :: Prog ()
testArr4 = do
    arr :: Arr Word32 Int32 <- initArr [8,7,6,5]
    iarr <- freezeArr arr 4
    sequence_ [printf "%d " $ iarr #! i | i' <- [0..3], let i = fromInteger i']
    printf "\n"

testArr5 :: Prog ()
testArr5 = do
    arr :: Arr Word32 Int32 <- initArr [8,7,6,5]
    iarr <- unsafeFreezeArr arr
    sequence_ [printf "%d " $ iarr #! i | i' <- [0..3], let i = fromInteger i']
    printf "\n"

testSwap1 :: Prog ()
testSwap1 = do
    arr1 :: Arr Word32 Int32 <- initArr [1,2,3,4]
    arr2 :: Arr Word32 Int32 <- initArr [11,12,13,14]
    unsafeSwap arr1 arr2
    sequence_ [getArr i arr1 >>= printf "%d " | i <- map fromInteger [0..3]]
    printf "\n"

testSwap2 :: Prog ()
testSwap2 = do
    arr1 :: Arr Word32 Int32 <- initArr [1,2,3,4]
    n <- fget stdin
    arr2 :: Arr Word32 Int32 <- newArr n
    copyArr arr2 arr1 4
    setArr 2 22 arr2
    unsafeSwap arr1 arr2
    sequence_ [getArr i arr1 >>= printf "%d " | i <- map fromInteger [0..3]]
    printf "\n"
    sequence_ [getArr i arr2 >>= printf "%d " | i <- map fromInteger [0..3]]
    printf "\n"

testIf1 :: Prog ()
testIf1 = do
    inp :: CExp Int32 <- fget stdin
    a <- ifE (inp #== 10)        (return (inp+1)) (return (inp*3))
    b <- ifE (not_ (inp #== 10)) (return (a+1))   (return (a*3))
    printf "%d %d\n" a b

testIf2 :: Prog ()
testIf2 = do
    inp :: CExp Int32 <- fget stdin
    iff (inp #== 11)        (printf "== 11\n") (printf "/= 11\n")
    iff (not_ (inp #== 11)) (printf "/= 11\n") (printf "== 11\n")
    iff (inp #== 12)        (printf "== 12\n") (return ())
    iff (not_ (inp #== 12)) (return ())        (printf "== 12\n")
    iff (inp #== 13)        (printf "== 13\n") (return ())
    iff (not_ (inp #== 13)) (return ())        (printf "== 13\n")
    iff (inp #== 14)        (return ())        (return ())

-- Loop from 0 to 9 in steps of 1
testFor1 :: Prog ()
testFor1 = for (0,1,9) $ \i ->
    printf "%d\n" (i :: CExp Int8)

-- Loop from 9 to 0 in steps of 2
testFor2 :: Prog ()
testFor2 = for (9,-2,0) $ \i ->
    printf "%d\n" (i :: CExp Int8)

-- Loop from 0 to but excluding 10 in steps of 2
testFor3 :: Prog ()
testFor3 = for (0, 2, Excl 10) $ \i ->
    printf "%d\n" (i :: CExp Int8)

-- While loop tested in the `sumInput` in Demo.hs.

testAssert :: Prog ()
testAssert = do
    inp :: CExp Int32 <- fget stdin
    assert (inp #> 0) "input too small"
    printf "past assertion\n"

testPtr :: Prog ()
testPtr = do
    addInclude "<stdlib.h>"
    addInclude "<string.h>"
    addInclude "<stdio.h>"
    addDefinition printArr_def
    p :: Ptr Int32 <- newPtr
    callProcAssign p "malloc" [valArg (100 :: CExp Word32)]
    arr :: Arr Word32 Int32 <- initArr [34,45,56,67,78]
    callProc "memcpy" [ptrArg p, arrArg arr, valArg (5*4 :: CExp Word32)]  -- sizeof(int32_t) = 4
    callProc "printf" [strArg "%d\n", deref $ ptrArg p]
    callProc "printArr" [ptrArg p]
    callProc "free" [ptrArg p]
  where
    printArr_def = [cedecl|
        void printArr (int * p) {
            printf("%d %d %d %d %d\n", p[0], p[1], p[2], p[3], p[4]);
        }
        |]

testArgs :: Prog ()
testArgs = do
    addInclude "<stdio.h>"
    addDefinition setPtr_def
    addDefinition ret_def
    let v = 55 :: CExp Int32
    r <- initRef (66 :: CExp Int32)
    a :: Arr Int32 Int32 <- initArr [234..300]
    p :: Ptr Int32 <- newPtr
    o <- newObject "int" False
    op <- newObject "int" True
    callProcAssign p "setPtr" [refArg r]
    callProcAssign o "ret" [valArg v]
    callProcAssign op "setPtr" [refArg r]
    callProc "printf"
        [ strArg "%d %d %d %d %d %d\n"
        , valArg v
        , deref (refArg r)
        , deref (arrArg a)
        , deref (ptrArg p)
        , objArg o
        , deref (objArg op)
        ]
  where
    setPtr_def = [cedecl|
        int * setPtr (int *a) {
            return a;
        }
        |]
    ret_def = [cedecl|
        int ret (int a) {
            return a;
        }
        |]

testExternArgs :: Prog ()
testExternArgs = do
    let v = 55 :: CExp Int32
    externProc "val_proc" [valArg v]
    r <- initRef v
    externProc "ref_proc1" [refArg r]
    externProc "ref_proc2" [deref $ refArg r]  -- TODO Simplify
    a :: Arr Int32 Int32 <- newArr 10
    externProc "arr_proc1" [arrArg a]
    externProc "arr_proc2" [addr $ arrArg a]
    externProc "arr_proc3" [deref $ arrArg a]
    p :: Ptr Int32 <- newPtr
    externProc "ptr_proc1" [ptrArg p]
    externProc "ptr_proc2" [addr $ ptrArg p]
    externProc "ptr_proc3" [deref $ ptrArg p]
    o <- newObject "int" False
    externProc "obj_proc1" [objArg o]
    externProc "obj_proc2" [addr $ objArg o]
    op <- newObject "int" True
    externProc "obj_proc3" [objArg op]
    externProc "obj_proc4" [addr $ objArg op]
    externProc "obj_proc5" [deref $ objArg op]
    let s = "apa"
    externProc "str_proc1"  [strArg s]
    externProc "str_proc2"  [deref $ strArg s]
    return ()



----------------------------------------

testAll = do
    compareCompiled  testTypes  "0\n"
    compareCompiled  testRef    ""
    compareCompiledM testCExp   "44\n"
    compareCompiled  testArr1   ""
    compareCompiled  testArr2   "20\n"
    compareCompiled  testArr3   ""
    compareCompiled  testArr4   ""
    compareCompiled  testArr5   ""
    compareCompiled  testSwap1  ""
    compareCompiled  testSwap2  "45\n"
    compareCompiled  testIf1    "12\n"
    compareCompiled  testIf2    "12\n"
    compareCompiled  testFor1   ""
    compareCompiled  testFor2   ""
    compareCompiled  testFor3   ""
    compareCompiled  testAssert "45"
    runCompiled      testPtr
    runCompiled      testArgs
    compileAndCheck  testExternArgs
  where
    compareCompiledM = compareCompiled'
        defaultExtCompilerOpts {externalFlagsPost = ["-lm"]}

