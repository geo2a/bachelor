import Control.Monad
import Control.Monad.Reader

adder :: ReaderT String (Reader Int) Int
adder = do
  str <- ask
  num <- ask
  return $ num + read str

f2 :: ReaderT Int (Reader String) Int
f2 = do
  str <- lift ask
  num <- ask
  return $ num + read str

runnerForAdder = runReader (runReaderT adder "1") 1

runnerForF2 = runReader (runReaderT f2 1) "1"
