module Main where

import Data.List

nucleotideComplement :: Char -> Char
nucleotideComplement 'A' = 'T'
nucleotideComplement 'T' = 'A'
nucleotideComplement 'C' = 'G'
nucleotideComplement 'G' = 'C'

main = do
  contents <- getContents
  putStrLn $ foldl' accumulateReversedNucleotideComplement [] contents
  
  where
    accumulateReversedNucleotideComplement = flip $ (:) . nucleotideComplement