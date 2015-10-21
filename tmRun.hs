import Prelude hiding (log)
import Control.Monad
import TM as T hiding (step)
import qualified CTM as C hiding (step)
import Mecha
import System.Environment
import Data.Map.Strict hiding (null,map,filter)

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
  args <- getArgs
  let
    ver = "v"`elem`args
    out = "o"`elem`args
    res = "r"`elem`args
    clk = "c"`elem`args
    phase = if clk
      then 2
      else if res
        then 1
        else 0
    mode = if out
      then 2
      else if ver
        then 1
        else 0
    m = construct t r s
    rm = restrict m
    cm = C.clockwisize rm
  case mode of
    0 -> case phase of
      0 -> proc m 0
      1 -> proc rm 0
      2 -> proc cm 0
    1 -> case phase of
      0 -> trace m
      1 -> trace rm
      2 -> trace cm
    2 -> putStrLn $ case phase of
      0 -> stringify m
      1 -> stringify rm
      2 -> stringify rm
