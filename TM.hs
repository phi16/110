module TM where

import Prelude hiding (lookup,log)
import Data.Map.Strict
import Control.Applicative

data Tape = Tape {
  left :: [Char],
  cur :: Char,
  right :: [Char]
}
type State = String
data Direction = L | R
type Transition = Map (String,Char) (Maybe (Direction,Char,String))
data Machine = Machine Tape State Transition

handle :: String -> Maybe a -> Either String a
handle s Nothing = Left s
handle s (Just x) = Right x

blank :: Char
blank = ' '

moveLeft, moveRight :: Tape -> Char -> Tape
moveLeft (Tape [] _ r) p = Tape [] blank (p:r)
moveLeft (Tape (l:ls) _ r) p = Tape ls l (p:r)
moveRight (Tape l _ []) p = Tape (p:l) blank []
moveRight (Tape l _ (r:rs)) p = Tape (p:l) r rs

instance Show Tape where
  show (Tape l c r) = "{" ++ reverse l ++ ['[',c,']'] ++ r ++ "}"

instance Show Machine where
  show (Machine t s r) = show t ++ (' ':s)

step :: Machine -> Either String Machine
step (Machine t s r) = do
  e <- handle ("No candidate : " ++ show (s,cur t)) $ let
      p = lookup (s,cur t) r
      q = lookup (s,'*') r
    in p <|> q
  (d,c,s') <- handle "Done" e
  let
    c' = if c == '*' then cur t else c
    t' = case d of
      L -> moveLeft t c'
      R -> moveRight t c'
  return $ Machine t' s' r

construct :: String -> Transition -> State -> Machine
construct ts r s = Machine (Tape (reverse $ init ts) (last ts) []) s r

run :: Machine -> ([Machine],String)
run m = case step m of
  Left e -> ([m],e)
  Right p -> let
      (ms,e) = run p
    in (m:ms,e)

log :: [Machine] -> String -> [String]
log [] r = [r]
log (x:xs) r = show x : log xs r

trace :: Machine -> IO ()
trace m = mapM_ putStrLn $ uncurry log $ run m

