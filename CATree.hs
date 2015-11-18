{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module CATree where

import qualified Data.Map.Strict as M
import Data.Monoid
import Control.Applicative

import Debug.Trace

data MCGen = MCGen Int Int (M.Map (Int,Int) (Int,Int))
  deriving Show
data RepTape = Bin [Int] | Rep Integer [RepTape]
  deriving Show
trans :: String -> RepTape
trans xs = Bin $ map f xs where
  f '0' = 0
  f '1' = 1
instance Monoid RepTape where
  mempty = Bin []
  x`mappend`y = Rep 1 [x,y]
  mconcat xs = Rep 1 xs

data MC a = MC {runMC :: MCGen -> (MCGen,a)}
instance Functor MC where
  fmap f (MC u) = MC $ \i -> fmap f $ u i
instance Applicative MC where
  pure x = MC $ \i -> (i,x)
  MC f <*> MC x = MC $ \i -> let
      (j,f') = f i
      (k,x') = x j
    in (k,f' x')
instance Monad MC where
  return x = MC $ \i -> (i,x)
  MC y >>= f = MC $ \i -> let
      (j,y') = y i
    in runMC (f y') j

makeTree :: RepTape -> MCGen
makeTree u = fst $ runMC (e 0 [u]) $ MCGen 2 1 M.empty where
  e :: Int -> [RepTape] -> MC ()
  e i r = doubling r >>= \case
    (xs,Nothing) -> succRank >> e (i+1) xs
    ([],Just d)
      | i >= 3 -> return ()
      | otherwise -> do
        z <- putTree d 0
        succRank
        e (i+1) [Bin [z]]
    (xs,Just d) -> do
      z <- putTree d 0
      succRank
      e (i+1) $ xs ++ [Bin [z]]

putTree :: Int -> Int -> MC Int
putTree 0 0 = return 0
putTree x y = MC $ \(MCGen i r m) -> case M.lookup (x,y) m of
  Nothing -> let
      m' = M.insert (x,y) (i,r) m
    in (MCGen (i+1) r m',i)
  Just (p,_) -> (MCGen i r m,p)
succRank :: MC ()
succRank = MC $ \(MCGen i r m) -> (MCGen i (r+1) m,())

doubling :: [RepTape] -> MC ([RepTape],Maybe Int)
doubling [] = return ([],Nothing)
doubling [Bin [d]] = return ([],Just d)
doubling (Bin []:ys) = doubling ys
doubling (Bin [x]:Bin []:ys) = doubling $ Bin [x]:ys
doubling (Bin [x]:Bin xs:ys) = doubling $ Bin (x:xs):ys
doubling (Bin [x]:Rep _ []:ys) = doubling $ Bin [x]:ys
doubling (Bin [x]:Rep n y:ys)
  | n == 0 = doubling $ Bin [x]:ys
  | otherwise = let
      (x':res) = y
      y' = res ++ [x']
    in doubling $ Bin [x]:x':Rep (n-1) y':res++ys
doubling (Bin (x:y:xs):ys) = do
  z <- putTree x y
  doubling (Bin xs:ys) >>= \case
    (zs,d) -> return (Bin [z]:zs,d)
doubling (Rep n xs:ys)
  | n == 0 = doubling ys
  | n == 1 = doubling $ xs++ys
  | n`mod`2 == 1 = doubling $ Rep (n-1) xs:xs++ys
  | otherwise = doubling xs >>= \case
    (xs',Nothing) -> doubling ys >>= \case
      (ys',d) -> return (Rep n xs':ys',d)
    (xs',Just p) -> doubling (Bin [p]:xs) >>= \case
      (xs'',Nothing) -> do
        let reps = Rep (n`div`2) $ xs'++xs''
        doubling ys >>= \case
          (ys',d) -> return (reps:ys',d)
      (_,Just _) -> error "This shouldn't happen..."

