import Data.List (partition)

data Terrain = [[Cube]]

data Cube = Empty
          | Lava
          | Trench {color : Int}

data Instruction = Instruction {dir :: Direction, num :: Int, color :: RGB}

data RGB = RGB {red   :: Int, green :: Int, blue  :: Int} 
    deriving (Show, Eq)

data Direction = Up | Down | Left | Right 
    deriving (Show, Eq)

fillRow :: [Cube] -> [Cube]
fillRow n = case n of
    Empty:ns          -> Empty : fillRow ns
    Empty:Lava:_      -> undefined

    Trench:Empty:ns   -> Trench : fillRow (Lava:ns)
    Trench:Trench:ns  -> n
    Trench:Lava:_     -> undefined

    Lava:Empty:ns     -> Lava : fillRow (Lava:ns)
    Lava:Trench:ns    -> ns
    Lava:Lava:_       -> undefined

    []                -> []

dig :: [Instruction] -> [Cube]
dig i = case i of
    Instruction{num = 0, ..} : ns              -> dig ns
    Instruction{dir = Left | Right, n, c} : ns -> dig Instruction{dir, n - 1, c} : ns
    Instruction{dir = Up | Down, n, c} : ns    -> dig Instruction{dir, n - 1, c} : ns
    []                                         -> []

parseInput :: String -> [Instruction]

partOne :: String -> Int
partOne input = count $ filter (= Lava | Trench) (dig $ parseInput input)