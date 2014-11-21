module Main where

getCount :: String -> String -> Int
getCount [] [] = 0
getCount (x:xs) (y:ys) = (getCount xs ys) + if x == y then 0 else 1

main = do
  contents <- getContents
  let contentLines = lines contents
  putStrLn $ show $ getCount (contentLines !! 0) (contentLines !! 1)
