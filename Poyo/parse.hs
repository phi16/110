import Control.Applicative
import System.IO
import System.Directory

po :: String -> IO String
po xs = do
  ys <- readFile xs
  return $ filter (/='\n') ys

filt :: Char -> Char
filt '0' = 'b'
filt '1' = 'o'

main :: IO ()
main = do
  tH <- openFile "temp.txt" WriteMode
  d <- pa 0 tH
  hClose tH
  iW <- openFile "out.rle" WriteMode
  iH <- openFile "temp.txt" ReadMode
  hPutStr iW "x = "
  hPutStr iW $ show d
  hPutStrLn iW ", y = 1, rule = W110"
  e <- hGetLine iH
  hPutStrLn iW e
  hClose iW
  hClose iH
  removeFile "temp.txt"

pa :: Integer -> Handle -> IO Integer
pa d h = do
  xs <- getLine
  if xs /= ""
    then do
      zs <- if length (words xs) == 1
        then po xs
        else do
          let [y,c] = words xs
          ys <- po y
          return $ concat $ replicate (read c) ys 
      hPutStr h $ map filt zs
      pa ((+d) $ fromIntegral $ length zs) h
    else return d
