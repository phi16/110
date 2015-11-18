{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module CA where

import qualified CTS as T
import CATapes
import CATree
import qualified Mecha as M
import Prelude hiding (log,lookup,words)
import Data.Monoid
import qualified Data.Map.Strict as Mi
import qualified Data.Set as Si
import Data.Word
import Data.Array (elems)
import Data.List (genericLength,genericTake)
import Control.Applicative hiding (empty)

data Binary = O | I deriving (Eq, Show)
data Machine = Machine MCGen

instance Show Machine where
  show xs = "{Machine}"
instance M.Mecha Machine where
  step = const $ Left "Inexecutable"
  stringify (Machine a) = unlines $ ps:he:cs where
    ps = "[M2] (golly 0.9)"
    he = "#R w110"
    cs = a`seq`[""]

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
index :: State Integer
index = State $ \(i,m) -> ((i,m),i)
rewrite :: (Integer,Integer) -> State ()
rewrite (p,q) = State $ \_ -> ((p,q),())
repi :: Integer -> State RepTape -> State RepTape
repi x a
  | x < 30 = repu x a
  | otherwise = do
    p <- repu 30 a
    d <- repu (x`mod`30) a
    return $ Rep (x`div`30) [p]`mappend`d
repu :: Integer -> State RepTape -> State RepTape
repu 0 a = return mempty
repu n a = do
  i <- a
  j <- repu (n-1) a
  return $ i`mappend`j

arrays :: [(Integer,[RepTape])]
arrays = map (\(d,e) -> (d,map trans e)) [
  (0,caTapeA),(2,caTapeB),(11,caTapeC),
  (13,caTapeD),(21,caTapeE),(15,caTapeF),
  (26,caTapeG),(22,caTapeH),(8,caTapeI),
  (14,caTapeJ),(0,caTapeK),(29,caTapeL)]

convert :: [Elem] -> RepTape
convert ls = simplify $ snd $ runState (s ls) (0,3) where
  s :: [Elem] -> State RepTape
  s [] = return mempty
  s (Ci:xs) = let
      (ix,ar) = arrays !! 2
    in do
      i <- increase 0
      rewrite ((fromIntegral i+ix)`mod`30,30)
      mappend (ar!!i) <$> s xs
  s (Ri n ys:xs) = do
    v <- repi n $ s ys
    vs <- s xs
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
  simplify xs = poi $ simpl xs
  simpl :: RepTape -> [RepTape]
  simpl (Bin []) = []
  simpl (Bin xs) = [Bin xs]
  simpl (Rep 0 xs) = []
  simpl (Rep 1 xs) = concatMap simpl xs
  simpl (Rep n xs) = [Rep n $ concatMap simpl xs]
  poi [] = Bin []
  poi [x] = x
  poi xs = Rep 1 xs

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
