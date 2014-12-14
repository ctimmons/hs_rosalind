module Main where

import Data.List

toString :: [[Int]] -> String
toString nss = drop 1 $ concat sortedSubListStrings
  where
    subListToString = \ns -> foldl' (\acc n -> acc ++ show n ++ " ") "\n" ns
    unsortedSubListStrings = map subListToString nss
    sortedSubListStrings = sort unsortedSubListStrings
      
main = do
  contents <- getContents
  let n = read contents
  let perm = permutations [1..n]
  putStrLn $ show $ length perm
  putStrLn $ toString perm
