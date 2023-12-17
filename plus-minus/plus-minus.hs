solve :: [Int] -> [Float]
solve xs = map f [p, n, z]
  where
    p = countElems (> 0) xs
    n = countElems (< 0) xs
    z = countElems (== 0) xs
    f = (`divif` length xs)

countElems :: (a -> Bool) -> [a] -> Int
countElems f xs = length $ filter f xs

divif :: Int -> Int -> Float
divif a b = fromIntegral a / fromIntegral b

readInput :: String -> [Int]
readInput = map read . tail . words

writeOutput :: [Float] -> String
writeOutput = unlines . map show

main :: IO ()
main = interact $ writeOutput . solve . readInput