module Main where

import Data.List

type Acc = (Int, Int, Int, Int)

nucleotideCount :: Acc -> Char -> Acc
nucleotideCount (a, c, g, t) 'A' = (a + 1, c, g, t)
nucleotideCount (a, c, g, t) 'C' = (a, c + 1, g, t)
nucleotideCount (a, c, g, t) 'G' = (a, c, g + 1, t)
nucleotideCount (a, c, g, t) 'T' = (a, c, g, t + 1)

toString :: Acc -> String
toString (a, c, g, t) = show a ++ " " ++ show c ++ " " ++ show g ++ " " ++ show t

main = do
  contents <- getContents
  putStrLn $ toString $ foldl' nucleotideCount (0, 0, 0, 0) contents
