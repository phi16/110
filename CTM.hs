module CTM where

import qualified TM as T
import qualified Mecha as M
import Prelude hiding (lookup,log)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Map.Strict hiding (map,foldr,filter,mapMaybe)
import Control.Monad (join)

data Tape = Tape {
  left :: [Char] -> [Char],
  cur :: Char,
  right :: [Char]
}
type State = String
data Write a = One a | Two a a deriving Eq
type Transition = Map (String,Char) (Write Char,String)
data Machine = Machine Tape State Transition State 

instance Show Tape where
  show (Tape l c r) = let
      str = r ++ l [] 
      li = length str
      le = li`div`2
      (he,tl) = splitAt le str
    in "{" ++ tl ++ ['[',c,']'] ++ he ++ "}"

instance Show Machine where
  show (Machine t s r e) = show t ++ (' ':s)

instance Show a => Show (Write a) where
  show (One x) = show x
  show (Two x y) = show x ++ show y

move :: Write Char -> Tape -> Tape
move (One x) (Tape l _ []) = let
    p:ps = (l.(x:)) []
  in Tape id p ps 
move (One x) (Tape l _ (r:rs)) = Tape (l.(x:)) r rs
move (Two x y) (Tape l _ []) = let
    p:ps = (l.(y:).(x:)) []
  in Tape id p ps
move (Two x y) (Tape l _ (r:rs)) = Tape (l.(x:).(y:)) r rs

step :: Machine -> Either String Machine
step (Machine t s r e) 
  | s == e = Left "Done"
  | otherwise = case lookup (s,cur t) r of
    Nothing -> Left $ "No candidate : " ++ show (s,cur t)
    Just (w,s') -> Right $ Machine (move w t) s' r e
 
construct :: [Char] -> Transition -> State -> State -> Machine
construct (t:ts) r s e = Machine (Tape id t ts) s r e 

instance M.Mecha Machine where
  step = step
  stringify m@(Machine t s r e) = unlines $ (show m ++ " -> " ++ e) : map show (assocs r)

clockwisize :: T.Machine -> Machine
clockwisize (T.Machine (T.Tape lt ct rt) s r) = Machine t' s r'' en where
  en = "[End]"
  t' = Tape (('>':'<':reverse lt)++) ct rt
  states = nub $ map (\(a,b,c) -> c) $ catMaybes $ elems r
  symbols = '<':'>':'*':(nub $ filter (/=' ') $ map snd $ keys r)
  translate ((su,ci),c) = case c of
    Nothing -> []
    Just (d,ch,st) -> case d of
      T.L -> let
            lef = ((su,'<'),(Two '<' '*','[':ch:']':st))
            rig = ((su,'>'),(Two '*' ch,"[>]" ++ st))
            dir = ((su,ci),(One '*','[':ch:']':st))
            def = if ci == ' ' then [lef,rig] else [dir]
            exs = [ (('[':s:']':st,t),(One s,'[':t:']':st)) | s <- symbols, t <- symbols, s /= '*', t /= '*']
          in def ++ exs
      T.R -> if ci == ' '
        then let
            lef = ((su,'<'),(Two '<' ch,st))
            rig = ((su,'>'),(Two ch '>','*':st))
            rigs = [(('*':st,s),(One s,'*':st)) | s <- symbols, s /= '>', s /= '*']
          in lef : rig : rigs
        else [((su,ci),(One ch,st))]
  r' = fromList $ toList r >>= translate
  defL = fromList $ do
    st <- states
    sy <- filter (/='*') symbols
    case lookup (st,sy) r' of
      Just (pc,ps) -> [(('[':sy:']':st,'*'),(pc,ps))]
      Nothing -> if st == en
        then [(('[':sy:']':st,'*'),(One sy,en))]
        else []
  defR = fromList  $ do
    st <- states
    case lookup (st,'>') r' of
      Just (pc,ps) -> [(('*':st,'>'),(pc,ps))]
      Nothing -> if st == en
        then [(('*':st,'>'),(One '>',en))]
        else []
  r'' = union r' $ union defL defR