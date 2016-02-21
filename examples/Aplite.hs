import Haste
import Haste.Aplite

prog :: Int32 -> Int32 -> IO Int32
prog = aplite defaultTuning $ \a b -> do
  done <- initRef false
  sum <- initRef (0 :: CExp Int32)
  while (not_ <$> getRef done) $ do
    setRef done true
    x <- getRef sum
    setRef sum (b-x+x+x+x+x `xor` x*a)
  getRef sum


main = do
  x <- prog 5 10
  putStrLn (show x)
