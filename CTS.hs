{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module CTS where

import qualified CTM as C
import qualified Mecha as M
import Prelude hiding (log,lookup)
import Data.List (group,nub)
import Data.Map.Strict (keys,fromList,lookup,toList)
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
  show (Machine t (Words p l v)) = poi ++ (' ':show ((p+l-1)`mod`l) ++ show (v!((p+l-1)`mod`l))) where
    poi = take 40 $ show t

rot :: Integer -> Words -> ([Binary],Words)
rot 0 (Words p s w) = (snd $ w!p,Words ((p+1)`mod`s) s w)
rot k (Words p s w) = rot 0 $ Words ((p+k)`mod`s) s w

endMachine :: [Binary] -> Bool
endMachine _ = False

step :: Machine -> Either String Machine
step (Machine (Tape [] d) ws) = case d [] of
  [] -> Left "Done"
  p -> step $ Machine (Tape p id) ws
step (Machine (Tape (O 1:xs) d) ws) = let
    (_,wi) = rot 0 ws
  in Right $ Machine (Tape xs d) wi
step (Machine (Tape (O 0:xs) d) ws) = step $ Machine (Tape xs d) ws
step (Machine (Tape (O n:xs) d) ws) = step $ Machine (Tape (O 1:O (n-1):xs) d) ws
step (Machine (Tape (I:xs) d) ws) = let
    (v,wi) = rot 0 ws
  in if endMachine v
    then Left "Done"
    else Right $ Machine (Tape xs $ d . (v++)) wi

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
    miu = Machine (Tape xs' d') wi
  in case eff miu of
    Left _ -> Right (ni,miu)
    Right (d,m') -> Right (ni+d,m')
eff (Machine (Tape (I:xs) d) ws) = let
    (v,wi) = rot 0 ws
  in if endMachine v
    then Left "Done"
    else Right $ (1,Machine (Tape xs $ d . (v++)) wi)

construct :: [Binary] -> Array Integer (String,[Binary]) -> Machine
construct xs vs = Machine (Tape xs id) w where
    w = Words 0 (succ $ uncurry subtract $ bounds vs) vs

trp :: String -> [Binary]
trp xs = group xs >>= \ys -> case head ys of
  '1' -> replicate (length ys) I
  '0' -> [O $ fromIntegral $ length ys]

instance M.Mecha Machine where
  step = step
  eff = eff
  stringify m@(Machine _ w) = unlines $ show m : ws where
    ws = map (\(z,(a,b)) -> concat[show z," : ",a," - ",show b]) $ zip [0..] $ case w of
      Words p s w' -> elems w'

tagSystemize :: C.Machine -> Machine
tagSystemize (C.Machine (C.Tape lT cT rT) st r e) = let
    symbols = nub $ map snd $ keys r
    states = filter (/="[End]") $ nub $ map fst $ keys r
    symPP = fromList $ flip zip [1..] symbols
    staPP = fromList $ flip zip [1..] states
    sym x = fromJust $ lookup x symPP
    sta q = fromJust $ lookup q staPP

    opt [] = []
    opt (I:xs) = I:opt xs
    opt (O n:O m:xs) = opt $ O (n+m):xs
    opt (O n:xs) = O n:opt xs

    s = fromIntegral $ length symbols
    q = fromIntegral $ length states
    bZ = 3*s+3
    cZ = bZ*3
    z = cZ*(q+2)+1+bZ
    poi s n d = (concat s,[O n,I,O $ d-n-1])
    st1 q = poi ["[1-",q,"]"] (cZ*sta q+bZ*2) $ 2*z
    st1' q = poi ["[1'-",q,"]"] (cZ*sta q+bZ*2+1) $ 2*z+bZ
    st2 q = poi ["[2-",q,"]"] (cZ*sta q+bZ+2) $ 2*z+bZ
    st3 q = poi ["[3-",q,"]"] (cZ*sta q+bZ+3) $ z+cZ*sta q+bZ*2
    sg1 q = poi ["[1g-",q,"]"] (cZ*sta q+bZ*2+4) $ 2*z
    sg1' q = poi ["[1'g-",q,"]"] (cZ*sta q+bZ*2+5) $ 2*z+bZ
    sg2 q = poi ["[2g-",q,"]"] (cZ*sta q+bZ+6) $ 2*z+bZ
    sg3 q = poi ["[3g-",q,"]"] (cZ*sta q+bZ+7) $ z+cZ*sta q+bZ*3
    sy s = poi ["(",[s],")"] (sym s*3) $ 2*z
    syM s = poi ["(-",[s],"-)"] (sym s*3+1) $ 2*z
    sy' s = poi ["(",[s],"')"] (sym s*3+2) z
    mu = poi ["<mu>"] 0 z
    muM = poi ["<-mu->"] 1 $ 2*z
    mu' = poi ["<mu'>"] 2 $ 2*z
    di = poi ["[Di]"] (4*bZ-1) $ 2*z+4*bZ
    pad n = ("{Pad}",[O n])
    nil _ = non
    non = ("{Null}",[])
    con xs = let (a,b) = unzip xs in (concat a,concat b)

    eArray = listArray (0,2*z-1) $ repeat $ nil ()
    changes = let
        poyo xs = symbols >>= \s -> map ($s) xs
        ori1 = zip [0..] $      [muM,muM,mu'] ++ poyo [sy ,syM,nil]
        ori2 = zip [bZ..] $     [non,muM,mu'] ++ poyo [sy',syM,nil]
        ori3 = zip [bZ*2..] $   [non,muM,mu ] ++ poyo [nil,syM,sy ]
        cem1 = zip [z..] $      [mu',muM,mu'] ++ poyo [sy ,syM,nil]
        cem2 = zip [z+bZ..] $   [non,muM,mu'] ++ poyo [sy ,syM,nil]
        cem3 = zip [z+bZ*2..] $ [non,muM,mu ] ++ poyo [nil,syM,syM]
        o d (t,p) = ("{Pad}"++t,O d:p)
        ori q = zip [cZ*sta q+bZ*2..] $ map ($q) [
          st1',st2,o (2*z-bZ*2).st1,nil,
          sg1',sg2,o (2*z-bZ*2).sg1,nil]
        cem q = zip [z+cZ*sta q+bZ*2..] $ map ($q) [
          o z.sg1',st3,nil,const $ pad (2*z-cZ*sta q-bZ*3),
          o z.sg1',sg3,nil,const $ pad (2*z-cZ*sta q-bZ*4)]
        trs = toList r >>= \((ks,kc),(wv,ws)) -> let
            idx = cZ*sta ks+3*sym kc+cZ
            ids = idx+bZ
            ns = st1 ws
            ads = case wv of
              C.One sp -> con [sy sp]
              C.Two sp sh -> con [sy sp,sy sh]
            dq = con [ads,ns]
            de = con [di,dq]
            end = con [ads,poi ["[Done1]"] (2*z-1-bZ) $ 2*z-bZ]
          in if ws == "[End]"
            then [(idx,end),(ids,end),(idx+1,sy kc),(idx+bZ+1,sy kc)] -- Finish Condition
            else [(idx,de),(ids,dq),(idx+1,sy kc),(idx+bZ+1,sy kc)]
        ams = states >>= \q -> let
            def = [(cZ*sta q+bZ*3+1,mu),(cZ*sta q+bZ*4+1,mu),(cZ*sta q+bZ*3+2,mu),(cZ*sta q+bZ*4+2,mu),(cZ*sta q+bZ*6,st1 q)]
            sbs = symbols >>= \s -> [(cZ*sta q+3*sym s+1,sy s),(cZ*sta q+3*sym s+bZ+1,sy s)]
          in def ++ sbs
        po = zip [bZ*4,bZ*4+3..] $ con [mu,mu] : map sy symbols
        po2 = [
          (z+bZ*4,con [mu,mu]),(bZ*4-1,pad $ 2*z-bZ*4),
          (2*z-1-bZ,poi ["[Done2]"] (2*z-1) $ 2*z+1)]
        endf = symbols >>= \s -> if s == '_' then [] else [(2*z-bZ+3*sym s,sy s)]
      in concat [ori1,ori2,ori3,cem1,cem2,cem3,states>>=ori,states>>=cem,trs,ams,po,po2,endf]
    ts = cT : rT ++ lT []
    ts' = (2^) $ ceiling $ logBase 2 $ fromIntegral $ length ts
    defTape = snd $ con $ concat [[st1 st],map sy ts,replicate ts' mu]
    resPats = ixmap (0,2*z-1) pred $ eArray // changes
  in construct [I] $ resPats // [(0,("[InitTape]",defTape))]