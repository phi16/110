import TM
import Data.Map.Strict hiding (null)

got :: String -> Char
got "_" = ' '
got (x:xs) = x
got [] = error "Bad String"

parse :: State -> [String] -> Maybe ((String,Char), Maybe (Direction,Char,String))
parse t [n,"->",c,s,d] = Just ((t,got n),Just (which d,got c,s)) where
  which "Left" = L
  which "Right" = R
parse t [n,"."] = Just ((t,got n),Nothing)
parse t e = Nothing

makeMap :: State -> Transition -> IO (Transition,State)
makeMap s m = do
  xs <- getLine
  if null xs
    then makeMap s m
    else if head xs == '-'
      then makeMap (tail $ dropWhile (/=' ') xs) m
      else case parse s $ words xs of
        Nothing -> return (m,xs)
        Just p -> makeMap s $ uncurry insert p m

main :: IO ()
main = do
  (r,s) <- makeMap "" empty
  t <- getLine
  let m = construct t r s
  trace m

