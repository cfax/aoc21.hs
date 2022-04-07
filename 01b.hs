main = do
    content <- readFile "input01"
    let descents = map read $ lines content :: [Int]
        averageDescents = zipWith3 (\x y z -> x+y+z)
            descents (tail descents) (tail $ tail descents)
    putStr $ show $ length $ filter (> 0) $ zipWith (-) (tail $ averageDescents) averageDescents
