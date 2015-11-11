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

e5A1, e5A2, e5A3, e5A4, e5B1, e5B2, e5B3, e5B4 :: SizeTape
e5C1, e5C2, e5C3, e5C4, e5D1, e5D2, e5D3, e5D4 :: SizeTape
e2A1, e2A2, e2A3, e2A4, e2B1, e2B2, e2B3, e2B4 :: SizeTape
e2C1, e2C2, e2C3, e2C4, e2D1, e2D2, e2D3, e2D4 :: SizeTape
e4A1, e4A2, e4A3, e4A4, e4B1, e4B2, e4B3, e4B4 :: SizeTape
e4C1, e4C2, e4C3, e4C4, e4D1, e4D2, e4D3, e4D4 :: SizeTape
e5A1 = convert "1111100000000100110101110011000100110"
e5A2 = convert "1111100010000000110111111101011100110"
e5A3 = convert "1111100010011000000111110000011111010"
e5A4 = convert "11100000110001000011000"
e5B1 = convert "1111101000011100110001110011000100110"
e5B2 = convert "1111100011100011010111001101011100110"
e5B3 = convert "1111100010011010011111110101111111010"
e5B4 = e5C1
e5C1 = convert "1111101100000111110000011111000100110"
e5C2 = convert "1111100011110000110001000011000100110"
e5C3 = convert "1111100010011001000111001100011100110"
e5C4 = convert "11101100110101110011010"
e5D1 = convert "1111101111011111110101111111000100110"
e5D2 = convert "1111100011100111000001111100000100110"
e5D3 = convert "1111100010011010110100001100010000110"
e5D4 = convert "11111111110001110011000"

e2A1 = convert "111110000000010011000100110"
e2A2 = convert "111110001000000011011100110"
e2A3 = convert "111110001001100000011111010"
e2A4 = convert "1110000011000"
e2B1 = convert "111110100001110011000100110"
e2B2 = convert "111110001110001101011100110"
e2B3 = convert "111110001001101001111111010"
e2B4 = e2C1
e2C1 = convert "1111101100000"
e2C2 = convert "111110001111000011000100110"
e2C3 = convert "111110001001100100011100110"
e2C4 = convert "1110110011010"
e2D1 = convert "111110111101111111000100110"
e2D2 = convert "111110001110011100000100110"
e2D3 = convert "111110001001101011010000110"
e2D4 = convert "1111111111000"

e4A1 = convert "11111000000001001101011100110"
e4A2 = convert "11111000100000001101111111010"
e4A3 = convert "11111000100110000001111100000"
e4A4 = convert "11100000110001000011000100110"
e4B1 = convert "11111010000111001100011100110"
e4B2 = convert "11111000111000110101110011010"
e4B3 = convert "1111100010011010011111110101111111000100110"
e4B4 = e4C1
e4C1 = convert "11111011000001111100000100110"
e4C2 = convert "11111000111100001100010000110"
e4C3 = convert "11111000100110010001110011000"
e4C4 = convert "11101100110101110011000100110"
e4D1 = convert "11111011110111111101011100110"
e4D2 = convert "11111000111001110000011111010"
e4D3 = convert "11111000100110101101000011000"
e4D4 = convert "11111111110001110011000100110"

e'A1, e'A2, e'A3, e'A4, e'B1, e'B2, e'B3, e'B4 :: SizeTape
e'C1, e'C2, e'C3, e'C4, e'D1, e'D2, e'D3, e'D4 :: SizeTape
e'E1, e'E2, e'E3, e'E4, e'F1, e'F2, e'F3, e'F4 :: SizeTape
e'G1, e'G2, e'G3, e'G4, e'H1, e'H2, e'H3, e'H4 :: SizeTape
e'A1 = convert "111110000100011111010"
e'A2 = convert "111110001000110011000"
e'A3 = convert "11111000100110011101110011000100110"
e'A4 = convert "111011011101011100110"
e'B1 = convert "111110111111011111010"
e'B2 = convert "111110001110000111000"
e'B3 = convert "11111000100110100011010011000100110"
e'B4 = convert "111110011111011100110"
e'C1 = convert "111110001011000111010"
e'C2 = convert "111110001001111100110"
e'C3 = convert "11111000100110110001011111000100110"
e'C4 = convert "111111001111000100110"
e'D1 = convert "111110000101100100110"
e'D2 = convert "111110001000111110110"
e'D3 = convert "11111000100110011000111111000100110"
e'D4 = convert "111011100110000100110"
e'E1 = convert "111110111010111000110"
e'E2 = convert "111110001110111110100"
e'E3 = convert "11111000100110111000111011000100110"
e'E4 = e'F1
e'F1 = convert "111110100110111100110"
e'F2 = convert "111110001110111110010"
e'F3 = convert "11111000100110111000101111000100110"
e'F4 = e'G1
e'G1 = convert "111110100111100100110"
e'G2 = convert "111110001110110010110"
e'G3 = convert "11111000100110111101111111000100110"
e'G4 = convert "111110011100000100110"
e'H1 = convert "111110001011010000110"
e'H2 = convert "111110001001111111000"
e'H3 = convert "11111000100110110000010011000100110"
e'H4 = convert "111111000011011100110"

rightUnit :: Integer -> SizeTape
rightUnit d = mempty

automatonize :: Integer -> T.Machine -> Machine
automatonize d ts = Machine $ mconcat [leftUnit d, centerUnit, rightUnit d]
