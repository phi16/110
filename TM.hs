{-# LANGUAGE LambdaCase #-}

module TM where

import Prelude hiding (lookup,log)
import Data.Map.Strict hiding (map,foldr)
import Data.Maybe hiding (mapMaybe)
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

restrict :: Machine -> Machine
restrict (Machine (Tape l c _) s r) = let
    st = "[Start]"
    en = "[End]"
    state n = "[State" ++ show n ++ "]"
    ss = reverse l
    ss' = flip map (zip [1..] $ tail ss) $ \(t,x) ->
      insert (state $ t-1,blank) $ Just (R,x,state t)
    sp = insert ("[Start]",blank) $ Just (R,head ss,state 0)
    se = case lookup (s,c) r <|> lookup (s,'*') r of
      Just (Just p@(d,ch,s')) -> insert (state $ length ss - 1,blank) $ Just (d,if ch == '*' then c else ch,s')
      Nothing -> id
    r' = foldr ($) r $ sp:se:ss'
    r'' = flip fmap r' $ \case
      Nothing -> Just (R,'*',en)
      Just p -> Just p
    r''' = insert (en,'*') Nothing r''
    rP = flip fmap r''' $ \case
      Nothing -> Nothing
      Just p@(d,ch,ns)
        | ch == ' ' -> Just (d,'_',ns)
        | otherwise -> Just p
    rQ = flip mapKeys rP $ \(s,c) -> case c of
      ' ' -> (s,'_')
      _ -> (s,c)
    rR = union rP rQ
  in Machine (Tape [] blank []) st rR

