import Haste
import Haste.Aplite

prog :: Int32 -> Int32 -> Double -> Int32
prog = aplite defaultTuning $ \a b c -> do
  done <- initRef false
  sum <- initRef (0 :: CExp Int32)
  while (not_ <$> getRef done) $ do
    setRef done true
    x <- getRef sum
    setRef sum (b-x+x+x+x+x `xor` x*a + round_ (c :: CExp Double))
  getRef sum

main = putStrLn (toString $ prog 5 10 15)
