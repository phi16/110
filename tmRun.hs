{-# LANGUAGE LambdaCase #-}

import Prelude hiding (log)
import Control.Monad
import Control.Applicative ((<$>))
import System.Environment
import System.IO (isEOF)
import Data.Map.Strict hiding (null,map,filter)
import TM as T hiding (step)
import qualified CTM as C hiding (step)
import qualified CTS as T hiding (step)
import Mecha

got :: String -> Char
got "_" = ' '
got (x:xs) = x
got [] = error "Bad String"

parse :: State -> [String] -> Maybe ((String,Char), Maybe (Direction,Char,String))
parse t [n,"->",c,s,d] = Just ((t,got n),Just (which d,got c,s)) where
  which "Left" = L
  which "Right" = R
parse t [n,"."] = Just ((t,got n),Nothing)
parse t e = Nothing

makeMap :: State -> Transition -> IO (Transition,State)
makeMap s m = do
  xs <- getLine
  if null xs
    then makeMap s m
    else if head xs == '-'
      then makeMap (tail $ dropWhile (/=' ') xs) m
      else case parse s $ words xs of
        Nothing -> return (m,xs)
        Just p -> makeMap s $ uncurry insert p m

main :: IO ()
main = do
  (r,s) <- makeMap "" empty
  t <- getLine
  outLenStr <- isEOF >>= \case
    False -> Just <$> getLine
    True -> return Nothing
  args <- getArgs
  let
    ver = "v"`elem`args
    out = "o"`elem`args
    res = "r"`elem`args
    clk = "c"`elem`args
    tag = "t"`elem`args
    slw = "s"`elem`args
    fin = "f"`elem`args
    phase = if tag
      then 3
      else if clk
        then 2
        else if res
          then 1
          else 0
    mode = if out
      then 2
      else if ver
        then 1
        else 0
    le = read . last . words <$> outLenStr
    m = construct t r s
    rm = restrict m
    cm = C.clockwisize rm
    tm = T.tagSystemize cm $ (+2) <$> if fin then Nothing else le
    stepCount = if slw then 1 else 10000000
    inst :: Mecha a => a -> IO ()
    inst = [proc stepCount, trace, putStrLn . stringify] !! mode
  case phase of
    0 -> inst m
    1 -> inst rm
    2 -> inst cm
    3 -> inst tm
