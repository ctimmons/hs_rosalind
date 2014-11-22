module Main where

import Data.Map.Strict as Map

rnaCodon :: Map.Map String (Maybe String)
rnaCodon = Map.fromList
      [("UUU", Just "F"),
       ("CUU", Just "L"),
       ("AUU", Just "I"),
       ("GUU", Just "V"),
       ("UUC", Just "F"),
       ("CUC", Just "L"),
       ("AUC", Just "I"),
       ("GUC", Just "V"),
       ("UUA", Just "L"),
       ("CUA", Just "L"),
       ("AUA", Just "I"),
       ("GUA", Just "V"),
       ("UUG", Just "L"),
       ("CUG", Just "L"),
       ("AUG", Just "M"),
       ("GUG", Just "V"),
       ("UCU", Just "S"),
       ("CCU", Just "P"),
       ("ACU", Just "T"),
       ("GCU", Just "A"),
       ("UCC", Just "S"),
       ("CCC", Just "P"),
       ("ACC", Just "T"),
       ("GCC", Just "A"),
       ("UCA", Just "S"),
       ("CCA", Just "P"),
       ("ACA", Just "T"),
       ("GCA", Just "A"),
       ("UCG", Just "S"),
       ("CCG", Just "P"),
       ("ACG", Just "T"),
       ("GCG", Just "A"),
       ("UAU", Just "Y"),
       ("CAU", Just "H"),
       ("AAU", Just "N"),
       ("GAU", Just "D"),
       ("UAC", Just "Y"),
       ("CAC", Just "H"),
       ("AAC", Just "N"),
       ("GAC", Just "D"),
       ("UAA", Nothing),
       ("CAA", Just "Q"),
       ("AAA", Just "K"),
       ("GAA", Just "E"),
       ("UAG", Nothing),
       ("CAG", Just "Q"),
       ("AAG", Just "K"),
       ("GAG", Just "E"),
       ("UGU", Just "C"),
       ("CGU", Just "R"),
       ("AGU", Just "S"),
       ("GGU", Just "G"),
       ("UGC", Just "C"),
       ("CGC", Just "R"),
       ("AGC", Just "S"),
       ("GGC", Just "G"),
       ("UGA", Nothing),
       ("CGA", Just "R"),
       ("AGA", Just "R"),
       ("GGA", Just "G"),
       ("UGG", Just "W"),
       ("CGG", Just "R"),
       ("AGG", Just "R"),
       ("GGG", Just "G")]

protein :: String -> String
protein rna =
  case rnaCodon Map.! (take 3 rna) of
    Just aminoAcid -> aminoAcid ++ protein (drop 3 rna)
    Nothing -> ""

main = do
  contents <- getContents
  putStrLn $ protein contents
