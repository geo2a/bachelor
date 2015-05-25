import Control.Eff
import Control.Eff.Reader.Lazy

-- It won't compile witouth extensible-effects package, 
-- so you should put it into sandbox, init cabal and do
-- all the other necessary stuff.
-- But it works, I promise you.

adder :: (Member (Reader Int) r, Member (Reader String) r) => Eff r Int
adder = do
  num <- ask
  str <- ask
  return $ num + read str

runAdder = run $ runReader (runReader adder "2") (1 :: Int)

finalCoutdown :: 
      (Member Fail r
      ,  Member (State Int) r)
      =>  Eff r ()
finalCoutdown = do
  state <- get
  if state == (0 :: Int) 
  then die
  else put (state - 1) >> finalCoutdown 

runCountdown1 n = run $ runState (n :: Int) $ runFail $  finalCoutdown

runCountdown2 n = run $ runFail $ runState (n :: Int) $ finalCoutdown