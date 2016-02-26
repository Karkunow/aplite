import Haste
import Haste.Aplite

sumArrayAplite :: Arr Int32 Double -> CExp Int32 -> Aplite Double
sumArrayAplite a sz = do
  r <- initRef 0
  for (0, 1, Excl sz) $ \i -> do
    x <- getArr i a
    y <- getRef r
    setRef r (x+y)
  getRef r

sumArray :: IOUArray Int32 Double -> Int32 -> Double
sumArray = aplite defaultTuning sumArrayAplite

sumArrayText :: JSString
sumArrayText = compile defaultTuning sumArrayAplite

main = do
  arr <- newListArray (0, 9) [1..10]
  alert $ show $ sumArray arr 10
