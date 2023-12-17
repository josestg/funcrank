import Control.Monad (replicateM)
import Text.Printf (printf)

point :: Int -> Int -> Int
point x y
  | x > y = 1
  | otherwise = 0

solve :: [Int] -> [Int] -> Int
solve xs ys = sum $ zipWith point xs ys

readRatings :: IO [Int]
readRatings = do
  map read . words <$> getLine

main :: IO ()
main = do
  (a : b : _) <- replicateM 2 readRatings
  printf "%d %d" (solve a b) (solve b a)