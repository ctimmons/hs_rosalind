module Utils.Fasta (
    FastaMap,
    getFastaMap
  ) where

import Data.Char
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

getFastaMap :: String -> FastaMap
getFastaMap contents = snd $ foldl' buildFastaMap ("", Map.empty) (lines contents)
  