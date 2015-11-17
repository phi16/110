{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module CA where

import qualified CTS as T
import CATapes
import qualified Mecha as M
import Prelude hiding (log,lookup,words)
import Data.Monoid
import Data.Array
import Data.List (genericLength,genericTake)
import Control.Applicative

import Debug.Trace

data Binary = O | I deriving Eq
data SizeTape = Tape !Integer ![Binary]
data Machine = Machine SizeTape

instance Show Machine where
  show (Machine (Tape _ ts)) = "{" ++ map c ts ++ "}" where
    c O = '0'
    c I = '1'

step :: Machine -> Either String Machine
step (Machine (Tape n ts)) = Right $ Machine $ Tape (n+1) $ update O (O:ts) where
  update _ [] = []
  update d (x:y:rs) = c d x y : update x (y:rs)
  update d [x] = [c d x O] 
  c O I I = I
  c _ I O = I
  c _ O I = I
  c _ _ _ = O

instance M.Mecha Machine where
  step = step
  stringify (Machine (Tape l ts)) = unlines [ps,ui] where
    ps = "x = " ++ show l ++ ", y = 1, rule = W110"
    ui = map ci ts
    ci O = 'b'
    ci I = 'o'

instance Monoid SizeTape where
  mempty = Tape 0 []
  mappend (Tape x xs) (Tape y ys) = Tape (x+y) $ xs++ys
 
repli :: Integer -> [Binary] -> [Binary]
repli 0 xs = []
repli n xs = xs ++ repli (n-1) xs

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
  in ((u,m),fromIntegral u)
rewrite :: (Integer,Integer) -> State ()
rewrite (p,q) = State $ \_ -> ((p,q),())

trans :: String -> SizeTape
trans xs = Tape l $ map f xs where
  l = fromIntegral $ length xs
  f '0' = O
  f '1' = I
arrays :: [(Integer,[SizeTape])]
arrays = map (\(d,e) -> (d,map trans e)) [
  (0,caTapeA),(2,caTapeB),(11,caTapeC),
  (17,caTapeD),(9,caTapeE),(15,caTapeF),
  (4,caTapeG),(8,caTapeH),(22,caTapeI),
  (16,caTapeJ),(0,caTapeK),(1,caTapeL)]

convert :: [Elem] -> SizeTape
convert ls = trace (show ls) $ snd $ runState (s ls) (0,3) where
  s :: [Elem] -> State SizeTape
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
leftUnit = [Ri 649 [Ai],Bi,Ri 13 [Ai],Bi,Ri 11 [Ai],Bi,Ri 12 [Ai],Bi]
centerUnit [T.I] = [Fi,Gi]
centerUnit [T.O n] = [Ri n [Ei,Gi]]
centerUnit (T.I:xs) = Fi : Di : centerUnit xs
centerUnit (T.O n:xs) = Ri n [Ei,Di] : centerUnit xs
rightUnit xs = let
    rightU [] = []
    rightU ([]:xs) = Li : rightU xs
    rightU ((e:es):xs) = Ki : Hi : p e ++ r es ++ rightU xs
    p T.I = [Ii]
    p (T.O 1) = [Ji]
    p (T.O n) = [Ji, Ri (n-1) [Ii,Ji]]
    r [] = []
    r (T.I:xs) = Ii : Ii : r xs
    r (T.O n:xs) = Ri n [Ii,Ji] : r xs
  in case rightU xs of
    (Ki:xs) -> xs ++ [Ki]
    xs -> xs

automatonize :: Integer -> T.Machine -> Machine
automatonize d (T.Machine ini ws) = Machine $ convert $ concat [le,ce,re] where
  le = [Ri d leftUnit]
  ce = Ci : centerUnit [T.I]
  re = [Ri (d`div`3+1) $ rightUnit [[T.I,T.O 1,T.I]]]
