import Data.List (partition)

data SchematicObject = PartNumber { num :: [Char], xStart :: Int, xEnd, , y :: Int}
                     | StarSymbol {x :: Int, y :: Int}
                     | Symbol {x :: Int, y :: Int}

parseParts :: ([Char], Int, Int, Maybe PartNumber) -> [SchematicObject]             -- should be Int, Int
parseParts (chars, i, part) = case (chars, i, part) of
    -- End of the line
    ([], _, Nothing)               -> []
    ([], i, Just p)                -> [p]                                            -- add xEnd to p

    -- Skip periods
    ('.':ns, i, Nothing)           -> parseParts        ns (i + 1) Nothing           -- add fields
    ('.':ns, i, Just p)            -> p ++ parseParts   ns (i + 1) Nothing           -- add n to p

    -- Parse digits
    (n:ns, i, Nothing) | isDigit n -> parseParts        ns (i + 1) SchematicObject{} -- add fields
    (n:ns, i, Just p)  | isDigit n -> parseParts        ns (i + 1) (Just p)          -- add n to p

    -- Parse star symbols
    ('*':ns, i, Nothing)           -> parseParts        ns (i + 1) StarSymbol{}      -- add fields
    ('*':ns, i, Just p)            -> p ++ parseParts   ns (i + 1) StarSymbol{}      -- add fields

    -- Parse other symbols
    (n:ns, i, Nothing)             -> parseParts        ns (i + 1) Symbol{}          -- add fields
    (n:ns, i, Just p)              -> p ++ parseParts   ns (i + 1) Symbol{}          -- add n to p

allParts :: String -> [[Char]]
allParts input = map (++) (map parseParts (parseInput input))
    where parseInput = map (map (:[])) (lines input??)

partOne :: String -> Int
partOne input = sum $ filter isAdjacent (allParts input)

partTwo :: String -> Int