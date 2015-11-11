{-# LANGUAGE LambdaCase #-}

module CA where

import qualified CTS as T
import qualified Mecha as M
import Prelude hiding (log,lookup)
import Data.Monoid
import Data.List (genericLength)

data Binary = O | I deriving Eq
data SizeTape = Tape Integer [Binary]
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
  mappend (Tape x xs) (Tape y ys) = Tape (x+y) (xs++ys)

convert :: String -> SizeTape
convert l = Tape (genericLength l) $ flip map l $ \case
  '0' -> O
  '1' -> I

repli :: Integer -> [Binary] -> [Binary]
repli 0 xs = []
repli n xs = xs ++ repli (n-1) xs

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
leftUnit d = mulZ [(217,e),(d`div`3+1,clock)]

a3, c2A1, c2B2, ele :: SizeTape
a3 = convert "111110111000100110"
c2A1 = convert "11111000000100110"
c2B2 = convert "11111000111011010"
ele = mulZ [(1,c2A1),(2,e),(1,c2A1),(2,e),(1,c2A1),(1,e),(1,c2B2)]
centerUnit :: SizeTape
centerUnit = mulZ [(216,e),(1,ele),(1,e),(1,a3)]

rightUnit :: Integer -> SizeTape
rightUnit d = mempty

automatonize :: Integer -> T.Machine -> Machine
automatonize d ts = Machine $ mconcat [leftUnit d, centerUnit, rightUnit d]
