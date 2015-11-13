{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module CA where

import qualified CTS as T
import qualified Mecha as M
import Prelude hiding (log,lookup,words)
import Data.Monoid
import Data.Array
import Data.List (genericLength)

data Binary = O | I | So | Si deriving Eq
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
  mappend (Tape x xs) (Tape y ys) = Tape (x+y) $ simpl $ xs++ys where
    simpl [] = []
    simpl (x:xs) = case simpl xs of
      [] -> [x]
      (So:ys) -> if x == O then ys else x:So:ys
      (Si:ys) -> if x == I then ys else x:Si:ys
      (O:ys) -> if x == So then ys else x:O:ys
      (I:ys) -> if x == Si then ys else x:I:ys
 
convert :: String -> SizeTape
convert l = Tape (genericLength l) $ flip map l $ \case
  '0' -> O
  '1' -> I

repli :: Integer -> [Binary] -> [Binary]
repli 0 xs = []
repli n xs
  | n > 0 = xs ++ repli (n-1) xs
  | n < 0 = [So,Si,Si,So,So,Si,So, So,So,Si,Si,Si,Si,Si] ++ repli (n+1) []

mult :: Integer -> SizeTape -> SizeTape
mult n (Tape t ts) = Tape (t*n) $ repli n ts
mulZ :: [(Integer, SizeTape)] -> SizeTape
mulZ xs = mconcat $ map (uncurry mult) xs

e, a41, a42, a43, clock :: SizeTape
e = convert "11111000100110"
a41 = convert "111110111011111000100110"
a42 = convert "111110001110111000100110"
a43 = convert "1110100110"
a4F1 = mulZ [(1,a41),(27,e),(1,a43),(23,e),(1,a42),(25,e),(1,a41)]
a4F2 = mulZ [(1,a42),(27,e),(1,a41),(23,e),(1,a43),(25,e),(1,a42)]
a4F3 = mulZ [(1,a43),(27,e),(1,a42),(23,e),(1,a41),(25,e),(1,a43)]
clock = mulZ [(649,e),(1,a4F2),(649,e),(1,a4F1),(649,e),(1,a4F3)]
leftUnit :: Integer -> SizeTape
leftUnit d = mulZ [(217,e),(d`div`3+1,clock),(216,e)]

a3, c2A1, c2B2, ele :: SizeTape
a3 = convert "111110111000100110"
c2A1 = convert "11111000000100110"
c2B2 = convert "11111000111011010"
ele = mulZ [(1,c2A1),(2,e),(1,c2A1),(2,e),(1,c2A1),(1,e),(1,c2B2)]
centerUnit :: SizeTape
centerUnit = mconcat [ele,e,a3]

e5, e2, e4 :: [SizeTape]
e5R, e2R, e4R :: Integer -> SizeTape
e5 = map convert [
  "1111100000000100110101110011000100110",
  "1111100010000000110111111101011100110",
  "1111100010011000000111110000011111010",
  "1110000011000100001100011111000100110",
  "1111101000011100110001110011000100110",
  "1111100011100011010111001101011100110",
  "1111100010011010011111110101111111010",
  "1111101100000111110000011111000100110",
  "1111100011110000110001000011000100110",
  "1111100010011001000111001100011100110",
  "1110110011010111001101011111000100110",
  "1111101111011111110101111111000100110",
  "1111100011100111000001111100000100110",
  "1111100010011010110100001100010000110",
  "1111111111000111001100011111000100110"]
e2 = map convert [
  "111110000000010011000100110",
  "111110001000000011011100110",
  "111110001001100000011111010",
  "111110001001101110000011000",
  "111110100001110011000100110",
  "111110001110001101011100110",
  "111110001001101001111111010",
  "111110110000011111000100110",
  "111110001111000011000100110",
  "111110001001100100011100110",
  "111110001001101110110011010",
  "111110111101111111000100110",
  "111110001110011100000100110",
  "111110001001101011010000110",
  "111110001001101111111111000"]
e4 = map convert [
  "11111000000001001101011100110",
  "11111000100000001101111111010",
  "11111000100110000001111100000",
  "11100000110001000011000100110",
  "11111010000111001100011100110",
  "11111000111000110101110011010",
  "11111000100110100111111101011",
  "11111011000001111100000100110",
  "11111000111100001100010000110",
  "11111000100110010001110011000",
  "11101100110101110011000100110",
  "11111011110111111101011100110",
  "11111000111001110000011111010",
  "11111000100110101101000011000",
  "11111111110001110011000100110"]
e5R x = e5 !! (fromIntegral x`mod`15)
e2R x = e2 !! (fromIntegral x`mod`15)
e4R x = e4 !! (fromIntegral x`mod`15)

e' :: [SizeTape]
e'R :: Integer -> SizeTape
e' = map convert [
  "111110000100011111010",
  "111110001000110011000",
  "011101110011000100110",
  "111011011101011100110",
  "111110111111011111010",

  "111110001110000111000",
  "100011010011000100110",
  "111110011111011100110",
  "111110001011000111010",
  "111110001001111100110",

  "1100010",
  "111111001111000100110",
  "111110000101100100110",
  "111110001000111110110",
  "0110001",

  "111011100110000100110",
  "111110111010111000110",
  "111110001110111110100",
  "111000111011000100110",
  "111110100110111100110",

  "111110001110111110010",
  "111000101111000100110",
  "111110100111100100110",
  "111110001110110010110",
  "1111011",

  "111110011100000100110",
  "111110001011010000110",
  "111110001001111111000",
  "110000010011000100110",
  "111111000011011100110"]
e'R x = e' !! (fromIntegral x`mod`30)

mergeE' :: Integer -> [Integer] -> [[Integer]] -> SizeTape
mergeE' d xs tbl = m (fromIntegral $ d`mod`30) $ map (e'R.(+d)) xs where
  m n xs = zipEther (map (flip mult e) $ tbl!!n) xs
  zipEther xs [] = mconcat xs
  zipEther (x:xs) (y:ys) = x`mappend`y`mappend`zipEther xs ys
initUnit :: Integer -> SizeTape
initUnit d = mconcat [e5R $ 9+d,e2R $ 6+d,e,e,e,e4R $ 9+d,e's] where
  e's = mergeE' d [2,6,10,7,3] $ [
    [2,1,2,1,2],[1,1,2,0,2],[1,1,2,0,2],
    [1,1,2,1,3],[2,1,3,1,2],[1,1,2,0,2],
    [1,1,2,0,2],[1,1,2,1,3],[2,2,3,0,2],
    [1,1,2,0,2],[1,1,2,0,2],[1,1,3,0,2],
    [2,2,2,0,2],[1,1,2,0,2],[1,1,3,1,1],
    [1,2,2,0,2],[1,1,2,0,2],[1,1,2,1,2],
    [1,2,3,0,2],[2,1,2,0,2],[1,1,2,0,2],
    [1,1,2,1,2],[2,2,2,0,2],[2,1,2,0,2],
    [1,1,2,0,2],[1,1,2,1,2],[2,1,2,0,2],
    [2,1,2,0,2],[1,1,2,0,2],[1,1,2,1,2]]
block1PUnit :: Integer -> SizeTape
block1PUnit d = mergeE' d [11,17,18,26,29,25,3,15,26,4,7,3] tbl where
  tbl = map (zipWith (+) [0,0,2,0,2,2,0,2,0,0,2,2]) $ [
    [0,0,0,0,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,0,0,0,0,0],
    [0,1,-1,1,-1,0,0,0,1,0,-1,0],[0,1,0,0,0,0,0,0,0,0,0,1],
    [0,1,-1,0,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,0,0,0,0,0],
    [0,1,0,1,-1,0,0,1,0,0,0,0],[0,1,0,0,0,0,0,1,0,0,0,1],
    [0,1,-1,0,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,1,0,0,0,0],
    [0,0,0,0,-1,0,0,1,0,0,0,0],[0,1,-1,0,0,1,0,1,0,0,0,0],
    [0,1,-1,0,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,1,-1,0,0,0],
    [0,0,0,0,0,0,0,1,0,1,0,-1],[0,1,-1,0,0,1,1,0,0,0,0,0],
    [0,1,-1,0,0,0,0,0,0,0,0,0],[0,0,-1,0,0,0,0,1,-1,1,0,0],
    [0,0,0,0,0,0,1,0,0,1,0,0],[0,1,-1,0,0,0,1,0,0,0,0,0],
    [0,1,-1,0,0,0,0,0,0,1,0,0],[0,0,-1,0,0,0,1,1,-1,1,0,0],
    [0,0,0,1,0,-1,1,0,0,0,0,0],[0,1,0,0,0,0,0,0,0,0,0,0],
    [0,1,-1,0,0,0,0,0,0,1,-1,0],[0,0,-1,1,0,0,1,0,1,0,0,0],
    [0,0,0,1,0,0,0,0,0,0,0,0],[0,1,0,0,0,0,0,0,0,0,0,0],
    [0,1,-1,1,0,0,0,0,1,1,-1,0],[0,1,-1,1,0,0,1,0,1,0,0,0]]
block1SUnit :: Integer -> SizeTape
block1SUnit d = mergeE' d [25,7,18,26,29,25,3,15,26,4,7,3] tbl where
  tbl = map (zipWith (+) [0,1,1,0,2,2,0,2,0,0,2,2]) $ [
    [0,1,-1,0,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,0,0,0,0,0],
    [0,1,-1,1,-1,0,0,0,1,0,-1,0],[0,1,0,0,0,0,0,0,0,0,0,1],
    [0,1,-1,0,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,0,0,0,0,0],
    [0,1,0,1,-1,0,0,1,0,0,0,0],[0,1,0,0,0,0,0,1,0,0,0,1],
    [0,1,-1,0,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,1,0,0,0,0],
    [0,1,0,0,-1,0,0,1,0,0,0,0],[0,1,-1,0,0,1,0,1,0,0,0,0],
    [0,1,-1,0,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,1,-1,0,0,0],
    [0,2,-1,0,0,0,0,1,0,1,0,-1],[0,2,-1,0,0,1,1,0,0,0,0,0],
    [0,1,-1,0,0,0,0,0,0,0,0,0],[0,2,-1,0,0,0,0,1,-1,1,0,0],
    [0,2,-1,0,0,0,1,0,0,1,0,0],[0,2,-1,0,0,0,1,0,0,0,0,0],
    [0,1,-1,0,0,0,0,0,0,1,0,0],[0,2,-2,0,0,0,1,1,-1,1,0,0],
    [0,2,-1,1,0,-1,1,0,0,0,0,0],[0,1,-1,0,0,0,0,0,0,0,0,0],
    [0,1,-1,0,0,0,0,0,0,1,-1,0],[0,2,-2,1,0,0,1,0,1,0,0,0],
    [0,1,-1,1,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,0,0,0,0,0],
    [0,1,-1,1,0,0,0,0,1,1,-1,0],[0,2,-2,1,0,0,1,0,1,0,0,0]]
block0Unit :: Integer -> SizeTape
block0Unit d = mergeE' d [19,1,12,20,23,19,3,15,26,4,7,3] tbl where
  tbl = map (zipWith (+) [0,2,0,0,2,2,2,2,0,0,2,2]) $ [
    [0,0,0,0,0,0,0,0,0,0,0,0],[0,1,-1,1,0,0,0,0,0,0,0,0],
    [0,0,0,1,0,0,-1,0,1,0,-1,0],[0,0,0,0,0,0,0,0,0,0,0,1],
    [0,0,0,1,0,0,0,0,0,0,0,0,0],[0,1,-1,1,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,-1,1,0,0,0,0],[0,0,0,0,0,0,0,1,0,0,0,1],
    [0,0,0,1,-1,0,0,0,0,0,0,0],[0,0,1,0,0,0,-1,1,0,0,0,0],
    [0,0,0,0,0,0,-1,1,0,0,0,0],[0,0,0,0,0,0,0,1,0,0,0,0],
    [0,0,1,1,-1,0,0,0,0,0,0,0],[0,0,1,0,0,0,-1,1,-1,0,0,0],
    [0,0,0,0,0,0,-1,1,0,1,0,-1],[0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,1,0,-1,0,0,0,0,0,0,0],[0,0,0,0,0,1,-1,1,-1,1,0,0],
    [0,0,0,0,0,0,0,0,0,1,0,0],[0,0,0,0,0,0,0,0,0,0,0,0],
    [0,1,0,0,0,0,0,0,0,1,0,0],[0,1,0,0,0,1,1,1,-1,1,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0],[0,1,0,0,0,0,0,0,0,0,0,0],
    [0,1,0,0,0,0,0,0,0,1,-1,0],[0,1,0,0,0,0,1,0,1,0,0,0],
    [0,0,0,0,0,0,0,0,0,0,0,0],[0,1,-1,0,0,0,0,0,0,0,0,0],
    [0,1,0,1,0,-1,0,0,1,1,-1,0],[0,0,0,0,0,0,0,0,1,0,0,0]]
data State a = State {runState :: Integer -> (Integer, a)}
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
increment :: Integer -> State Integer
increment d = State $ \i -> ((i+d)`mod`30,i)
iniU :: State SizeTape
bSU,bPU :: T.Binary -> State SizeTape
iniU = do
  d <- increment 0
  return $ initUnit d
bPU T.I = do
  d <- increment 18
  return $ block1PUnit d
bPU (T.O n) = do
  d <- increment 4
  return $ block0Unit (d+22)
bSU T.I = do
  d <- increment 1
  return $ block1SUnit (d+22)
bSU (T.O n) = do
  d <- increment 16
  return $ block0Unit (d+4)
periodUnit :: [T.Binary] -> State SizeTape
periodUnit a = do
  i <- iniU
  is <- case a of
    [] -> return mempty
    (au:as) -> (:) <$> bPU au <*> mapM bSU as
  return $ mconcat $ i:is
rightUnit :: Integer -> [[T.Binary]] -> SizeTape
rightUnit d a = snd $ runState (seq d $ cycle a) 0 where
  seq 0 _ = return mempty
  seq n (y:ys) = do
    x <- periodUnit y
    xs <- seq (n-1) ys
    return $ x`mappend`xs

automatonize :: Integer -> T.Machine -> Machine
automatonize d (T.Machine _ ws) = Machine $ mconcat [le,ce,re] where
  le = leftUnit d
  ce = centerUnit
  re = rightUnit d $ [[T.I],[T.I,T.O 1,T.I]]
  -- re = rightUnit d (T.wSize ws) $ fmap snd $ T.words ws
