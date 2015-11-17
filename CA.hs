{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module CA where

import qualified CTS as T
import CATapes
import qualified Mecha as M
import Prelude hiding (log,lookup,words)
import Data.Monoid
import qualified Data.Map as Mi
import qualified Data.Set as Si
import Data.Word
import Data.Array (elems)
import Data.List (genericLength,genericTake)
import Control.Applicative hiding (empty)

import Debug.Trace

data Binary = O | I deriving (Eq, Show)
data MCGen = MCGen (Si.Set Word8) (Mi.Map (Either (Word8,Word8) (Integer,Integer)) Integer) Integer
data Machine = Machine MCGen

instance Show Machine where
  show xs = "{Machine}"
instance M.Mecha Machine where
  step = const $ Left "Inexecutable"
  stringify (Machine a) = unlines $ ps:he:cs where
    ps = "[M2] (golly 0.9)"
    he = "#R w110"
    cs = a`seq`[""]

data RepTape = Bin [Binary] | Rep Integer [RepTape]
  deriving Show
trans :: String -> RepTape
trans xs = Bin $ map f xs where
  f '0' = O
  f '1' = I
instance Monoid RepTape where
  mempty = Bin []
  x`mappend`y = Rep 1 [x,y]

makeTree :: RepTape -> MCGen
makeTree u = trace (show u) $ MCGen Si.empty Mi.empty 0

data Elem = Ai | Bi | Ci | Di | Ei | Fi | Gi | Hi | Ii | Ji | Ki | Li | Ri Integer [Elem]
  deriving Show

data State a = State {runState :: (Integer,Integer) -> ((Integer,Integer), a)}
instance Functor State where
  fmap f (State u) = State $ \i -> fmap f $ u i
instance Applicative State where
  pure x = State $ \i -> (i,x)
  State f <*> State x = State $ \i -> let
      (j,f') = f i
      (k,x') = x j
    in (k,f' x')
instance Monad State where
  return x = State $ \i -> (i,x)
  State y >>= f = State $ \i -> let
      (j,y') = y i
    in runState (f y') j

increase :: Integer -> State Int
increase d = State $ \(i,m) -> let
    u = (i+d)`mod`m
  in ((u,m),fromIntegral i)
rewrite :: (Integer,Integer) -> State ()
rewrite (p,q) = State $ \_ -> ((p,q),())

arrays :: [(Integer,[RepTape])]
arrays = map (\(d,e) -> (d,map trans e)) [
  (0,caTapeA),(2,caTapeB),(11,caTapeC),
  (13,caTapeD),(21,caTapeE),(15,caTapeF),
  (26,caTapeG),(22,caTapeH),(8,caTapeI),
  (14,caTapeJ),(0,caTapeK),(29,caTapeL)]

convert :: [Elem] -> RepTape
convert ls = snd $ runState (s ls) (0,3) where
  s :: [Elem] -> State RepTape
  s [] = return mempty
  s (Ci:xs) = let
      (ix,ar) = arrays !! 2
    in do
      i <- increase 0
      rewrite ((fromIntegral i+ix)`mod`30,30)
      mappend (ar!!i) <$> s xs
  s (Ri 0 ys:xs) = s xs
  s (Ri n ys:xs) = do
    v <- s ys
    vs <- s $ Ri (n-1) ys:xs
    return $ v`mappend`vs
  s (x:xs) = let
      (ix,ar) = arrays !! case x of
        {Ai -> 0; Bi -> 1; Ci -> 2;
         Di -> 3; Ei -> 4; Fi -> 5;
         Gi -> 6; Hi -> 7; Ii -> 8;
         Ji -> 9; Ki -> 10; Li -> 11;}
    in do
      i <- increase ix
      mappend (ar!!i) <$> s xs

leftUnit :: [Elem]
centerUnit :: [T.Binary] -> [Elem]
rightUnit :: [[T.Binary]] -> [Elem]
leftUnit = [Ri 1049 [Ai],Bi,Ri 13 [Ai],Bi,Ri 11 [Ai],Bi,Ri 12 [Ai],Bi]
centerUnit [T.I] = [Fi,Gi]
centerUnit [T.O n] = [Ri n [Ei,Gi]]
centerUnit (T.I:xs) = Fi : Di : centerUnit xs
centerUnit (T.O n:xs) = Ri n [Ei,Di] : centerUnit xs
rightUnit xs = let
    rightU [] = []
    rightU ([]:xs) = Li : rightU xs
    rightU ((e:es):xs) = Ki : Hi : p e ++ r es ++ rightU xs
    p T.I = [Ii]
    p (T.O 0) = []
    p (T.O 1) = [Ji]
    p (T.O n) = [Ji, Ri (n-1) [Ii,Ji]]
    r [] = []
    r (T.I:xs) = Ii : Ii : r xs
    r (T.O n:xs) = Ri n [Ii,Ji] : r xs
  in case rightU xs of
    (Ki:xs) -> xs ++ [Ki]
    xs -> xs

automatonize :: Integer -> T.Machine -> Machine
automatonize d (T.Machine ini ws) = Machine $ makeTree $ convert $ concat [le,ce,re] where
  initT = [T.I] -- T.next ini
  wordT = [[T.I]] -- map snd $ elems $ T.words ws
  le = [Ri d leftUnit]
  ce = Ci : centerUnit initT
  re = [Ri ((d`div`genericLength wordT)+1) $ rightUnit wordT]
