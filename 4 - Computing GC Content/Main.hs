module Main where

import Data.Function
import qualified Data.Map.Strict as Map
import Utils.Fasta

gcContent :: String -> Float
gcContent s = ((length filteredContent) `intDiv` (length s)) * 100.0
  where
    filteredContent = filter (\c -> (c == 'C') || (c == 'G')) s
    intDiv = (/) `on` fromIntegral

maxValue :: (String, Float) -> String -> Float -> (String, Float)
maxValue acc key value =
  if value > snd acc then (key, value)
  else acc

toString :: (String, Float) -> String
toString (key, value) = displayKey ++ "\n" ++ show value
  where
    -- drop the leading '>' character.
    displayKey = drop 1 key

main = do
  contents <- getContents
  let gcContentMap = Map.map gcContent (getFastaMap contents)
  let result = Map.foldlWithKey' maxValue ("", 0.0) gcContentMap
  putStrLn $ toString result
  