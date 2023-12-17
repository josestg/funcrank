solve :: [String] -> Int
solve = sum . map read

main :: IO ()
main = interact $ show . solve . tail . words