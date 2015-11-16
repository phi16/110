{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module CA where

import qualified CTS as T
import qualified Mecha as M
import Prelude hiding (log,lookup,words)
import Data.Monoid
import Data.Array
import Data.List (genericLength,genericTake)
import Control.Applicative

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
 
convert :: String -> SizeTape
convert l = Tape (genericLength l) $ flip map l $ \case
  '0' -> O
  '1' -> I

repli :: Integer -> [Binary] -> [Binary]
repli 0 xs = []
repli n xs = xs ++ repli (n-1) xs

leftUnit :: Integer -> SizeTape
centerUnit :: [T.Binary] -> SizeTape
rightUnit :: [[T.Binary]] -> SizeTape
leftUnit = undefined
centerUnit = undefined
rightUnit = undefined

automatonize :: Integer -> T.Machine -> Machine
automatonize d (T.Machine ini ws) = Machine $ mconcat [le,ce,re] where
  le = leftUnit d
  ce = centerUnit [T.I]
  re = rightUnit $ genericTake d $ cycle [[T.I]]
