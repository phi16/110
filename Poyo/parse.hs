import Control.Applicative

po :: String -> IO String
po xs = do
  ys <- readFile xs
  return $ filter (/='\n') ys

filt :: Char -> Char
filt '0' = 'b'
filt '1' = 'o'

main :: IO ()
main = pa 0

pa :: Integer -> IO ()
pa d = do
  xs <- getLine
  if xs /= ""
    then do
      zs <- if length (words xs) == 1
        then po xs
        else do
          let [y,c] = words xs
          ys <- po y
          return $ concat $ replicate (read c) ys 
      putStr $ map filt zs
      pa $ (+d) $ fromIntegral $ length zs
    else putStr "\n" >> print d
