{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE LambdaCase #-}

module CTS where

import qualified CTM as C
import qualified Mecha as M
import Prelude hiding (log)
import Data.List (group)
import Data.Array

data Binary = O Int | I
data Tape = Tape {
  next :: [Binary],
  store :: [Binary] -> [Binary]
}
data Words = Words {
  point :: Integer,
  size :: Integer,
  words :: Array Integer [Binary]
}
data Machine = Machine Tape Words

instance {-# OVERLAPPING #-} Show [Binary] where
  show xs = concatMap i xs where
    i (O n) = replicate n '0' -- "0{" ++ show n ++ "}"
    i I = "1"

instance Show Tape where
  show (Tape r s) = "{" ++ show r ++ show (s []) ++ "}"

instance Show Machine where
  show (Machine t (Words p _ v)) = show t ++ (' ':show (v!p))

rot :: Integer -> Words -> ([Binary],Words)
rot 0 (Words p s w) = (w!p,Words ((p+1)`mod`s) s w)
rot k (Words p s w) = rot 0 $ Words ((p+k)`mod`s) s w

step :: Machine -> Either String Machine
step (Machine (Tape [] d) ws) = case d [] of
  [] -> Left "Done"
  p -> step $ Machine (Tape p id) ws
step (Machine (Tape (O 1:xs) d) ws) = let
    (_,wi) = rot 0 ws
  in Right $ Machine (Tape xs d) wi
step (Machine (Tape (O n:xs) d) ws) = step $ Machine (Tape (O 1:O (n-1):xs) d) ws
step (Machine (Tape (I:xs) d) ws) = let
    (v,wi) = rot 0 ws
  in Right $ Machine (Tape xs $ d . (v++)) wi

eff :: Machine -> Either String (Integer,Machine)
eff (Machine (Tape [] d) ws) = case d [] of
  [] -> Left "Done"
  p -> eff $ Machine (Tape p id) ws
eff (Machine (Tape (O n:xs) d) ws) = let
    ni = fromIntegral n
    (_,wi) = rot (ni-1) ws
  in Right $ (ni,Machine (Tape xs d) wi)
eff (Machine (Tape (I:xs) d) ws) = let
    (v,wi) = rot 0 ws
  in Right $ (1,Machine (Tape xs $ d . (v++)) wi)

construct :: [Char] -> [[Char]] -> Machine
construct xs vs = Machine (Tape (p xs) id) $ cons $ map p vs where
  cons us = Words 0 l $ listArray (0,l-1) us where
    l = fromIntegral $ length us
  p xs = group xs >>= \xs -> case head xs of
    '1' -> replicate (length xs) I
    '0' -> [O $ fromIntegral $ length xs]

instance M.Mecha Machine where
  step = step
  eff = eff
  stringify m@(Machine _ w) = unlines $ show m : map show ws where
    ws = case w of
      Words p s w' -> elems w'

tagSystemize :: C.Machine -> Machine
tagSystemize _ = construct "100100100" ["010001","100","100100100","","",""]
