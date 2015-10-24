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
    bZ = uZ*3
    z = bZ*(q+2) + 1 -- Maybe
    tap i a = opt [O i,I,O $ z-i-1]
    po e = (e,)
    poe xs = (concat xs,)

    symCode x = po ['[',x,']'] $ tap (sym x + 1) $ 2*z
    symMCode x = po ['<',x,'>'] $ tap (sym x + s + 1) $ 2*z
    mu = po "[mu]" $ [I,O $ z-1]
    mumu = po "[mu,mu]" $ snd mu ++ snd mu
    muM = po "{mu}" $ tap (s*2+2) $ 2*z
    muP = po "[mu\']" $ tap (s*2+3) $ 2*z
    symPCode x = po ['[',x,'\'',']'] $ tap (sym x + s*2 + 3) z
    state1 q = poe ["[1|",q,"]"] $ tap (bZ*sta q+uZ*2) $ 2*z
    stateG1 q = poe ["<1|",q,">"] $ tap (bZ*sta q+uZ*2+4) $ 2*z
    state1' q = poe ["[1'|",q,"]"] $ tap (bZ*sta q+uZ*2+1) $ 2*z+2*uZ
    stateG1' q = poe ["<1'|",q,">"] $ tap (bZ*sta q+uZ*2+5) $ 2*z+2*uZ
    state2 q = poe ["[2|",q,"]"] $ tap (bZ*sta q+uZ+2) $ 2*z+2*uZ
    stateG2 q = poe ["<2|",q,">"] $ tap (bZ*sta q+uZ+6) $ 2*z+2*uZ
    state3 q = poe ["[3|",q,"]"] $ tap (bZ*sta q+uZ+3) $ z+bZ*sta q+4*uZ
    stateG3 q = poe ["<3|",q,">"] $ tap (bZ*sta q+uZ+7) $ z+bZ*sta q+6*uZ
    di = po "[D]" $ tap 39 $ 2*z+40

    defTape = {- opt $ -} concat $ map snd $ state1 st : map symCode t ++ replicate s' mu where
      t = cT : rT ++ lT []
      s' = 2 ^ ceiling (logBase 2 $ fromIntegral $ length t)
    wsv = let
        none _ = ("[none]",[])
        ori = zip [0..] $ concat $ concat [hlv,cnt,mrk] where -- [0,bZ)
          hlv = [[muM],map symCode symbols,map symMCode symbols,[muM,muP],map none symbols]
          cnt = [[],map symPCode symbols,map symMCode symbols,[muM,muP],map none symbols]
          mrk = [[],map none symbols,map symMCode symbols,[muM,mu],map symCode symbols]
        oriS q = let qi = sta q in zip [bZ*qi+uZ*2..] $ concat [map ($q) ss,pad,sms] where
          ss = [state1',state2,ap.state1,nil,stateG1',stateG2,ap.stateG1]
          ap (v,bi) = ('+':v,O (2*z-uZ*2) : bi)
          pad = replicate (uZ-length ss) nil
          tri s = case lookup (q,s) r of
            Nothing -> none ()
            Just (One e,q') -> ("("++q++[',',s,')'],map snd $ concat [symCode e,state1 q'])
            Just (Two e f,q') -> ("("++q++[',',s,')'],map snd $ concat [di,symCode e,symCode f,state1 q'])
          tru s = case lookup (q,s) r of
            Nothing -> none ()
            Just (One e,q') -> ("("++q++[',',s,')'],map snd $ concat [symCode e,state1 q'])
            Just (Two e f,q') -> ("("++q++[',',s,')'],map snd $ concat [symCode e,symCode f,state1 q'])
          sms = [[],map tri symbols,map symCode symbols,[mu,[]],map none symbols]
          ses = [[],map tru symbols,map symCode symbols,[mu,[]],map none symbols]
        cem = zip [z..] $ concat $ concat [hlv,cnt,mrk] where -- [z,z+bZ)
          hlv = [[muP],map symCode symbols,map symMCode symbols,[muM,muP],map none symbols]
          cnt = [[],map symCode symbols,map symMCode symbols,[muM,muP],map none symbols]
          mrk = [[],map none symbols,map symMCode symbols,[muM,mu],map symMCode symbols]
        cemS =
      in concat $ concat [[ori,cem],map oriS states,map cemS states]
  in construct defTape $ array (0,2*z-1) wsv
