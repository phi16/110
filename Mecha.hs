{-# LANGUAGE TupleSections #-}

module Mecha where

import Prelude hiding (log)
import Control.Monad

class Show a => Mecha a where
  step :: a -> Either String a
  eff :: a -> Either String (Integer,a)
  eff x = fmap (1,) $ step x
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

proc :: Mecha a => a -> IO ()
proc m = procI m 0 0 where
  procI m d ds = case eff m of
    Left s -> do
      putStr $ show ds
      print m
      putStrLn s
    Right (n,m') -> do
      when (d`mod`1==0) $ do
        putStr $ show $ ds+n
        print m'
      procI m' (d+1) $! ds+n
