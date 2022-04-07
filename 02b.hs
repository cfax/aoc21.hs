data Direction = Up | Down | Forward

instance Show Direction where
    show Up = "up"
    show Down = "down"
    show Forward = "forward"

instance Read Direction where
    readsPrec _ "up" = [(Up, "")]
    readsPrec _ "down" = [(Down, "")]
    readsPrec _ "forward" = [(Forward, "")]

type HorizontalPosition = Int
type VerticalPosition = Int

data Position = Position {
    hPos :: HorizontalPosition,
    vPos :: VerticalPosition,
    aim :: Int}
    deriving Show

move :: Position -> Direction -> Int -> Position
move p Up n      = Position (hPos p) (vPos p) (aim p - n)
move p Down n    = Position (hPos p) (vPos p) (aim p + n)
move p Forward n = Position (hPos p + n) (vPos p + aim p * n) (aim p)

toTuple :: [String] -> (Direction, Int)
toTuple [d, n] = (read d :: Direction, read n :: Int)

main = do
    content <- readFile "input02"
    let moves = map (toTuple . words) $ lines content
        position = foldl (\p (d, n) -> move p d n) (Position 0 0 0) moves
    putStrLn $ show $ hPos position * vPos position
