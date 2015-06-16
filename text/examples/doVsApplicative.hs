import Control.Applicative
import Control.Monad.State

action :: IO String
action = do
  a <- getLine
  b <- getLine
  return $ a ++ b

action' :: IO String
action' = (++) <$> getLine <∗> getLine