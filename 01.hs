main = do
    content <- readFile "input01"
    let descents = map read $ lines content :: [Int]
    putStr $ show $ length $ filter (> 0) $ zipWith (-) (tail $ descents) descents
