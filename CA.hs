{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE LambdaCase #-}

module CA where

import qualified CTS as T
import qualified Mecha as M
import Prelude hiding (log,lookup)

data Binary = O | I deriving Eq
data Machine = Machine [Binary]

instance Show Machine where
  show (Machine ts) = "{" ++ map c ts ++ "}" where
    c O = '0'
    c I = '1'

step :: Machine -> Either String Machine
step (Machine ts) = Right $ Machine $ update O (O:ts) where
  update _ [] = []
  update d (x:y:rs) = c d x y : update x (y:rs)
  update d [x] = [c d x O] 
  c O I I = I
  c _ I O = I
  c _ O I = I
  c _ _ _ = O

instance M.Mecha Machine where
  step = step
  stringify (Machine ts) = unlines [ps,ui] where
    l = length ts
    ps = "x = " ++ show l ++ ", y = 1, rule = W110"
    ui = map ci ts
    ci O = 'b'
    ci I = 'o'

convert :: String -> [Binary]
convert = map $ \case
  '0' -> O
  '1' -> I

automatonize :: T.Machine -> Machine
automatonize ts = Machine [I]
