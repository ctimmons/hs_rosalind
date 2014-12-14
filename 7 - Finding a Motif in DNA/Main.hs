module Main where

import Data.List

motif :: String -> String -> [Int]
motif n h = recMotif 0 h
  where
    lastpos = (length h) - (length n)
    recMotif pos h =
      if pos > lastpos then []
      else (if n `isPrefixOf` h then [pos + 1] else []) ++ recMotif (pos + 1) (drop 1 h)

toString :: [Int] -> String
toString ns = drop 1 $ foldl' (\acc n -> acc ++ " " ++ show n) "" ns
      
main = do
  contents <- getContents
  let needleAndHaystack = lines contents
  let haystack = needleAndHaystack !! 0
  let needle = needleAndHaystack !! 1
  putStrLn $ toString $ motif needle haystack
