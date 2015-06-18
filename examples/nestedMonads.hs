import Control.Monad
import Control.Monad.State

fib :: Int -> State (Int,Int) Int
fib n = do
  forM_ [2..n] $ \i -> do
    (x,y) <- get
    put $ (y, x + y)
  snd `fmap` get >>= return

main = do -- монада IO
  n <- (readLn :: IO Int)
  return $ (flip evalState) (0,1) $ do -- монада State
    forM_ [2..n] $ \i -> do
      (x,y) <- get
      put $ (y, x + y)
    snd `fmap` get >>= return
