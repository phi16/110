module Mecha where

import Prelude hiding (log)
import Control.Monad

class Show a => Mecha a where
  step :: a -> Either String a
  stringify :: a -> String

run :: Mecha a => a -> ([a],String)
run m = case step m of
  Left e -> ([m],e)
  Right p -> let
      (ms,e) = run p
    in (m:ms,e)

log :: Show a => [a] -> String -> [String]
log [] r = [r]
log (x:xs) r = show x : log xs r

trace :: Mecha a => a -> IO ()
trace m = mapM_ putStrLn $ uncurry log $ run m

proc :: Mecha a => a -> Integer -> IO ()
proc m d = case step m of
  Left s -> do
    putStr $ show d
    print m
    putStrLn s
  Right m' -> do
    when (d`mod`1000000==0) $ do
      putStr $ show d
      print m'
    proc m' (d+1)
