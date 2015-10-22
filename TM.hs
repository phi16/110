{-# LANGUAGE LambdaCase #-}

module TM where

import qualified Mecha as M
import Prelude hiding (lookup,log)
import Data.List (nub,delete)
import Data.Map.Strict hiding (map,foldr,delete)
import Data.Maybe hiding (mapMaybe)
import Data.Either (partitionEithers)
import Control.Applicative

data Tape = Tape {
  left :: [Char],
  cur :: Char,
  right :: [Char]
}
type State = String
data Direction = L | R deriving Show
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

construct :: [Char] -> Transition -> State -> Machine
construct ts r s = Machine (Tape (reverse $ init ts) (last ts) []) s r

instance M.Mecha Machine where
  step = step
  stringify m@(Machine t s r) = unlines $ show m : map show (assocs r)

restrict :: Machine -> Machine
restrict (Machine t s r) = let
    en = "[End]"
    rP = flip fmap r $ \case
      Nothing -> Nothing
      Just p@(d,ch,ns)
        | ch == ' ' -> Just (d,'_',ns)
        | otherwise -> Just p
    rQ = flip mapKeys rP $ \(s,c) -> case c of
      ' ' -> (s,'_')
      _ -> (s,c)
    rR = union rP rQ
    r' = rR
    r'' = flip fmap r' $ \case
      Nothing -> Just (R,'*',en)
      Just p -> Just p
    r''' = insert (en,'*') Nothing r''
    symbols = delete '*' $ nub $ map snd $ keys r'''
    removeAsterisk ((s,'*'),Nothing)
      | s == en = map Right [((en,'*'),Nothing)]
      | otherwise = map Left [((s,c),Nothing) | c <- symbols]
    removeAsterisk ((s,'*'),Just (d,'*',s')) = map Left [((s,c),Just (d,if c == blank then '_' else c,s')) | c <- symbols]
    removeAsterisk ((s,'*'),Just p) = map Left [((s,c),Just p) | c <- symbols]
    removeAsterisk ((s,c),Nothing) = [Right ((s,c),Nothing)]
    removeAsterisk ((s,c),Just (d,'*',s')) = map Right [((s,c),Just (d,c,s'))]
    removeAsterisk e@((s,c),Just (d,ch,s')) = [Right e]
    (rA,rB) = partitionEithers $ toList r''' >>= removeAsterisk
    r'''' = fromList rB `union` fromList rA
  in Machine t s r''''

