{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module CTS where

import qualified CTM as C
import qualified Mecha as M
import Prelude hiding (log)

data Binary = O | I
data Tape = Tape {
  next :: [Binary],
  store :: [Binary] -> [Binary]
}
data Words = Words {
  rest :: [[Binary]],
  done :: [[Binary]] -> [[Binary]]
}
data Machine = Machine Tape Words

instance {-# OVERLAPPING #-} Show [Binary] where
  show xs = map i xs where
    i O = '0'
    i I = '1'

instance Show Tape where
  show (Tape r s) = "{" ++ show r ++ show (s []) ++ "}"

instance Show Machine where
  show (Machine t (Words [] d)) = show t ++ (' ':show (head $ d []))
  show (Machine t (Words (x:xs) _)) = show t ++ (' ':show x)

put :: Tape -> [Binary] -> Maybe (Binary,Tape)
put (Tape [] d) ys
  | (p:ps) <- d [] = Just (p,Tape ps (ys++))
  | otherwise = Nothing
put (Tape (x:xs) d) ys = Just (x,Tape xs $ d.(ys++))

rot :: Words -> ([Binary],Words)
rot (Words [] d) = rot $ Words (d []) id
rot (Words (x:xs) d) = (x, Words xs $ d.(x:))

step :: Machine -> Either String Machine
step (Machine ts ws) = let
    p@(~(Just (r,t'))) = put ts b
    (v,vs) = rot ws
    b = case r of
      I -> v
      O -> []
    m' = Machine t' vs
  in case p of
    Nothing -> Left "Done"
    Just _ -> Right m'

construct :: [Char] -> [[Char]] -> Machine
construct xs vs = Machine (Tape (p xs) id) $ Words (map p vs) id where
  p = map $ \case
    '1' -> I
    '0' -> O

instance M.Mecha Machine where
  step = step
  stringify m@(Machine _ w) = unlines $ show m : map show ws where
    ws = case w of
      Words rs dn -> rs ++ dn []

tagSystemize :: C.Machine -> Machine
tagSystemize _ = construct "100100100" ["010001","100","100100100","","",""]
