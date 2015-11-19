{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module CATree where

import qualified Data.Map.Strict as M
import Data.Monoid
import Control.Applicative
import Control.DeepSeq
import qualified Data.Sequence as Q
import Data.Sequence ((|>),(><),(<|),ViewL(..),viewl)
import GHC.Generics

import Debug.Trace

uni = Q.singleton

data MCGen = MCGen !Int !Int !(M.Map (Int,Int) (Int,Int))
  deriving (Show, Generic)
instance NFData MCGen
data RepTape = Bin !(Q.Seq Int) | Rep !Integer !(Q.Seq RepTape)
  deriving (Show, Generic)
instance NFData RepTape
trans :: String -> RepTape
trans xs = Bin $!! Q.fromList $!! map f xs where
  f '0' = 0
  f '1' = 1
instance Monoid RepTape where
  mempty = Bin Q.empty
  x`mappend`y = Rep 1 $!! Q.fromList [x,y]
  mconcat xs = Rep 1 $!! Q.fromList xs

data MC a = MC {runMC :: !(MCGen -> (MCGen,a))}
instance Functor MC where
  fmap f (MC u) = MC $ \i -> fmap f $ u i
instance Applicative MC where
  pure x = MC $ \i -> (force i,x)
  MC f <*> MC x = MC $ \i -> let
      (j,f') = f i
      (k,x') = x j
    in (force k,f' x')
instance Monad MC where
  return x = MC $ \i -> (force i,x)
  MC y >>= f = MC $ \i -> let
      (j,y') = y i
    in runMC (f y') $ force j

makeTree :: RepTape -> MCGen
makeTree (force -> u) = fst $!! runMC (e 0 $ uni u) $ MCGen 2 1 M.empty where
  e :: Int -> Q.Seq RepTape -> MC ()
  e i (force -> !r) = trace (show r) $ doubling r >>= \case
    (xs,Nothing) -> do
      succRank
      e (i+1) xs
    (xs,Just d) -> case Q.viewl xs of
      Q.EmptyL -> if i >= 3
        then return ()
        else do
          z <- putTree d 0
          succRank
          e (i+1) $ uni $ Bin $ uni z
      _ -> do
        z <- putTree d 0
        succRank
        e (i+1) $ xs |> Bin (uni z)

putTree :: Int -> Int -> MC Int
putTree 0 0 = return 0
putTree x y = MC $ \(MCGen i r m) -> case M.lookup (x,y) m of
  Nothing -> let
      m' = M.insert (x,y) (i,r) m
    in (MCGen (i+1) r m',i)
  Just (p,_) -> (MCGen i r m,p)
succRank :: MC ()
succRank = MC $ \(MCGen i r m) -> (MCGen i (r+1) m,())

doubling :: Q.Seq RepTape -> MC (Q.Seq RepTape,Maybe Int)
doubling q = case viewl q of
  EmptyL -> return (Q.empty,Nothing)
  (Bin v :< q') -> case viewl v of
    EmptyL -> doubling q'
    (x :< vs) -> case viewl vs of
      EmptyL -> case viewl q' of
        EmptyL -> return (Q.empty,Just x)
        (e :< ws) -> case e of
          Bin ys -> case viewl ys of
            EmptyL -> doubling $ Bin (uni x) <| ws
            _ -> doubling $ Bin (x <| ys) <| ws
          Rep n ys -> case viewl ys of
            EmptyL -> doubling $ Bin (uni x) <| ws
            (r :< rs)
              | n == 0 -> doubling $ Bin (uni x) <| ws
              | otherwise -> let
                  y' = rs |> r
                in doubling $ Bin (uni x) <| r <| Rep (n-1) y' <| rs >< ws
      (y :< xs) -> do
        z <- putTree x y
        doubling (Bin xs <| q') >>= \case
          (zs,d) -> return $ case viewl zs of
            EmptyL -> (uni $ Bin (uni z),d)
            (Bin zi :< zis) -> (Bin (z <| zi) <| zis,d)
            _ -> (Bin (uni z) <| zs,d)
  (Rep n xs :< ys) -> case viewl xs of
    EmptyL -> doubling ys
    _
      | n == 0 -> doubling ys
      | n == 1 -> doubling $ xs >< ys
      | n`mod`2 == 1 -> doubling $ Rep (n-1) xs <| xs >< ys
      | otherwise -> doubling xs >>= \case
        (xs',Nothing) -> doubling ys >>= \case
          (ys',d) -> return (Rep n xs' <| ys',d)
        (xs',Just p) -> doubling (Bin (uni p) <| xs) >>= \case
          (xs'',Nothing) -> do
            let reps = Rep (n`div`2) $ xs' >< xs''
            doubling ys >>= \case
              (ys',d) -> return (reps <| ys',d)
          (_,Just _) -> error "This shouldn't happen..."
