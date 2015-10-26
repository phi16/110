{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module CTS where

import qualified CTM as C
import qualified Mecha as M
import Prelude hiding (log,lookup)
import Data.List (group,nub)
import Data.Map.Strict (keys,fromList,lookup)
import Data.Maybe (fromJust)
import Data.Array hiding (fromList)

data Binary = O Integer | I deriving Eq
data Tape = Tape {
  next :: [Binary],
  store :: [Binary] -> [Binary]
}
data Words = Words {
  point :: Integer,
  wSize :: Integer,
  words :: Array Integer (String,[Binary])
}
data Machine = Machine Tape Words

instance {-# OVERLAPPING #-} Show [Binary] where
  show xs = concatMap i xs where
    i (O n) = "0{" ++ show n ++ "}"
    i I = "1"

instance Show Tape where
  show (Tape r s) = "{" ++ show r ++ show (s []) ++ "}"

instance Show Machine where
  show (Machine t (Words p _ v)) = show t ++ (' ':show (v!p))

rot :: Integer -> Words -> ([Binary],Words)
rot 0 (Words p s w) = (snd $ w!p,Words ((p+1)`mod`s) s w)
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
    (be,xsu) = span (/=I) xs
    (bn,xs',d') = if null xsu
      then let
          (br,bu) = span (/=I) $ d []
        in (br,bu,id)
      else ([],xsu,d)
    n' = n + sum (mi be) + sum (mi bn) where
      mi = map (\(O n) -> n)
    ni = fromIntegral n'
    (_,wi) = rot (ni-1) ws
  in Right $ (ni,Machine (Tape xs' d') wi)
eff (Machine (Tape (I:xs) d) ws) = let
    (v,wi) = rot 0 ws
  in Right $ (1,Machine (Tape xs $ d . (v++)) wi)

construct :: [Binary] -> Array Integer (String,[Binary]) -> Machine
construct xs vs = Machine (Tape xs id) w where
    w = Words 0 (uncurry subtract $ bounds vs) vs

trp :: String -> [Binary]
trp xs = group xs >>= \ys -> case head ys of
  '1' -> replicate (length ys) I
  '0' -> [O $ fromIntegral $ length ys]

instance M.Mecha Machine where
  step = step
  eff = eff
  stringify m@(Machine _ w) = unlines $ show m : ws where
    ws = map (\(a,b) -> a ++ " - " ++ show b) $ case w of
      Words p s w' -> elems w'

tagSystemize :: C.Machine -> Machine
tagSystemize (C.Machine (C.Tape lT cT rT) st r e) = let
    symbols = nub $ map snd $ keys r
    states = nub $ map fst $ keys r
    symPP = fromList $ flip zip [0..] symbols
    staPP = fromList $ flip zip [1..] states
    sym x = fromJust $ lookup x symPP
    sta q = fromJust $ lookup q staPP

    opt [] = []
    opt (I:xs) = I:opt xs
    opt (O n:O m:xs) = opt $ O (n+m):xs
    opt (O n:xs) = O n:opt xs

    s = fromIntegral $ length symbols
    q = fromIntegral $ length states
    uZ = 3*s+3
  in construct undefined $ array (0,undefined-1) undefined
