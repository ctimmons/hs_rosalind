module Main where

main = do
  contents <- getContents
  putStrLn $ map (\c -> if c == 'T' then 'U' else c) contents