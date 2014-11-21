module Main where

import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map

type FastaMap = Map.Map String String

isKey :: String -> Bool
isKey [] = False
isKey (c:cs) = c == '>'

buildFastaMap :: (String, FastaMap) -> String -> (String, FastaMap)
buildFastaMap (key, map) line =
  if all isSpace line then (key, map) {- No op. -}
  else
    if isKey line then (line, Map.insert line ""              map) {- New key without a value. -}
    else               (key,  Map.insert key  (value ++ line) map) {- Update current key's value by appending the new line of data to it. -}
  
  where
    value = map Map.! key

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
  let fastaMap = snd $ foldl' buildFastaMap ("", Map.empty) (lines contents)
  let gcContentMap = Map.map gcContent fastaMap
  let result = Map.foldlWithKey' maxValue ("", 0.0) gcContentMap
  putStrLn $ toString result
  