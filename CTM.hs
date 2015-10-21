module CTM where

import qualified TM as T
import qualified Mecha as M
import Prelude hiding (lookup,log)
import Data.Map.Strict hiding (map,foldr)

data Tape = Tape {
  left :: [Char] -> [Char],
  cur :: Char,
  right :: [Char]
}
type State = String
data Write a = One a | Two a a
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
move (Two x y) (Tape l _ (r:rs)) = Tape (l.(y:).(x:)) r rs 

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
  stringify m@(Machine t s r e) = unlines $ show m : ("End : " ++ e) : map show (assocs r)

clockwisize :: T.Machine -> Machine
clockwisize (T.Machine t s r) = Machine (Tape id '0' ['$']) "Po" tr "Pon" where
  tr = fromList [(("Po",'1'),(Two '1' '0',"Po")),(("Po",'0'),(One '1',"Po")),(("Po",'$'),(One '$',"Po"))]
