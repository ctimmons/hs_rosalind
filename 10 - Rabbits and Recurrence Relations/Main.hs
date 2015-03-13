module Main where

main = do
  contents <- getContents
  let n_and_k = words contents
  let n = read $ n_and_k !! 0
  let k = read $ n_and_k !! 1
  let raw_population_sequence = (\(a, b) -> (b, (a * k) + b)) `iterate` (1, 1)
  let final_population_value = fst $ last $ take n raw_population_sequence
  putStrLn $ show $ final_population_value
