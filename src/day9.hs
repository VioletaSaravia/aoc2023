import Data.List (partition)

main :: IO ()
main = print $ day9 $ parseInput $ getContents

day9 :: [[Int]] -> Int
day9 input = sum $ map (last . extrapolate . reverse . (\x -> x : diffsUntilZero x)) input

parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)

diffsUntilZero :: [Int] -> [[Int]]
diffsUntilZero x
  | null x = []
  | all (== 0) x = []
  | otherwise = (diffs x) : diffsUntilZero(diffs x)

diffs :: [Int] -> [Int]
diffs n = case n of
  (x:y:xs)  -> (y - x) : diffs (y:xs)
  [x]       -> []
  []        -> []

extrapolate :: [[Int]] -> [Int]
extrapolate n = case n of
  (x:y:xs) -> extrapolate ((y ++ [last x + last y]) : xs)
  [x]      -> x
  []       -> []