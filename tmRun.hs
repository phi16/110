{-# LANGUAGE LambdaCase #-}

import Prelude hiding (log)
import Control.Monad
import Control.Applicative ((<$>))
import System.Environment
import System.Directory
import System.IO
import Data.Map.Strict hiding (null,map,filter)
import Data.Maybe (fromMaybe)
import TM as T hiding (step)
import qualified CTM as C hiding (step)
import qualified CTS as T hiding (step)
import qualified CA as A hiding (step)
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

makeMap :: Handle -> State -> Transition -> IO (Transition,State)
makeMap hdl s m = do
  xs <- hGetLine hdl
  if null xs
    then makeMap hdl s m
    else if head xs == '-'
      then makeMap hdl (tail $ dropWhile (/=' ') xs) m
      else case parse s $ words xs of
        Nothing -> return (m,xs)
        Just p -> makeMap hdl s $ uncurry insert p m

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> dispHelp
    _ -> let fn = head args in do
      b <- doesFileExist fn
      case b of
        False -> do
          putStrLn $ "Error : " ++ show fn ++ " doesn't exist."
          putStrLn ""
          dispHelp
        True -> do
          u <- openFile fn ReadMode
          runTM u args

dispHelp :: IO ()
dispHelp = mapM_ putStrLn [
  "Usage : ./tmRun <filename> <options>*",
  "",
  "Machine Mode",
  "- {none} : Turing Machine",
  "- r : Restricted Turing Machine",
  "- c : Clockwise Turing Machine",
  "- t : Cyclic Tag System",
  "-- f : Don't create binaryEncoding",
  "-- i : Don't eliminate initialTape",
  "-- s : Efficient but 1Step",
  "- a : Rule 110 Automaton",
  "Running Option",
  "- o : Output Machine",
  "- v : Verbose Output"]

runTM :: Handle -> [String] -> IO ()
runTM hdl args = do
  (r,s) <- makeMap hdl "" empty
  t <- hGetLine hdl
  let
    maybeGetLine = hIsEOF hdl >>= \case
      False -> Just <$> hGetLine hdl
      True -> return Nothing
  outLenStr <- maybeGetLine
  stepLenStr <- maybeGetLine
  hClose hdl
  let
    res = "r"`elem`args
    clk = "c"`elem`args
    tag = "t"`elem`args
    ver = "v"`elem`args
    aut = "a"`elem`args
    out = "o"`elem`args
    slw = "s"`elem`args
    fin = "f"`elem`args
    ini = "i"`elem`args
    phase = if aut
      then 4
      else if tag
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
    slen = read . last . words <$> stepLenStr
    m = construct t r s
    rm = restrict m
    cm = C.clockwisize rm
    tm = T.tagSystemize cm ini $ (+2) <$> if fin then Nothing else le
    am = A.automatonize (fromMaybe 6 slen) tm
    stepCount = if slw then 1 else 1000000
    inst :: Mecha a => a -> IO ()
    inst = [proc stepCount, trace, putStrLn . stringify] !! mode
  case phase of
    0 -> inst m
    1 -> inst rm
    2 -> inst cm
    3 -> inst tm
    4 -> inst am
