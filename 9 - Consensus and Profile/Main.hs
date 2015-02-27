module Main where

import Data.List(foldl', intercalate, map, maximum)
import Data.Map.Strict(elems)
import Utils.Fasta

--                    A    C    G    T
type ProfileTuple = (Int, Int, Int, Int)

getProfileTuple :: Char -> ProfileTuple
getProfileTuple 'A' = (1, 0, 0, 0)
getProfileTuple 'C' = (0, 1, 0, 0)
getProfileTuple 'G' = (0, 0, 1, 0)
getProfileTuple 'T' = (0, 0, 0, 1)

getConsensusCharacter :: ProfileTuple -> Char
getConsensusCharacter (a, c, g, t) = if (a == max) then 'A' else
                                     if (c == max) then 'C' else
                                     if (g == max) then 'G' else
                                     if (t == max) then 'T' else 'A'
  where
    max = maximum [a, c, g, t]

mapDnaStringToProfileTuples :: String -> [ProfileTuple]
mapDnaStringToProfileTuples = map getProfileTuple

combineProfileTuples :: ProfileTuple -> ProfileTuple -> ProfileTuple
combineProfileTuples (a, c, g, t) (a', c', g', t') = (a + a', c + c', g + g', t + t')

getProfileRow :: Char -> [ProfileTuple] -> String
getProfileRow 'A' ps = "A: " ++ intercalate " " (map (\(a, _, _, _) -> show a) ps)
getProfileRow 'C' ps = "C: " ++ intercalate " " (map (\(_, c, _, _) -> show c) ps)
getProfileRow 'G' ps = "G: " ++ intercalate " " (map (\(_, _, g, _) -> show g) ps)
getProfileRow 'T' ps = "T: " ++ intercalate " " (map (\(_, _, _, t) -> show t) ps)

main = do
  contents <- getContents
  let fastaMap = getFastaMap contents
  let dnaStrings = elems fastaMap
  let profileTuples = map mapDnaStringToProfileTuples dnaStrings
  let profile = foldl' (zipWith combineProfileTuples) (head profileTuples) (tail profileTuples)
  let consensus = map getConsensusCharacter profile
  putStrLn $ intercalate "\n" [consensus, getProfileRow 'A' profile, getProfileRow 'C' profile, getProfileRow 'G' profile, getProfileRow 'T' profile]
