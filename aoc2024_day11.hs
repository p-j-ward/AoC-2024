-- Advent of Code 2024, Day 11
-- reads input from input_day11.txt, prints results to console

numDigits :: Int -> Int
numDigits x =
    let divd = div x 10 in (if divd == 0 then 1 else 1 + numDigits divd)

splitStone :: Int -> Int -> [Int]
splitStone x digs =
    let p = 10 ^ div digs 2 in [div x p, mod x p]

blinkSingleStone :: Int -> [Int]
blinkSingleStone s
    | s == 0 = [1]
    | even (numDigits s) = splitStone s digs
    | otherwise = [2024 * s]
    where digs = numDigits s

blinkStoneSequence :: [Int] -> [Int]
blinkStoneSequence stones =
    case stones of
        [] -> []
        _ -> blinkSingleStone (head stones) ++ blinkStoneSequence (tail stones)

blinkNTimes :: [Int] -> Int -> [Int]
blinkNTimes stones n =
    case n of
        0 -> stones
        1 -> blinkStoneSequence stones
        _ -> blinkNTimes (blinkStoneSequence stones) (n - 1)
main = do
    -- read and parse input into list
    inputString <- readFile "input_day11.txt"
    let inputList = map read (words (head (lines inputString)))  :: [Int]

    -- calculate number of stones after 25 blinks
    let ans1 = length (blinkNTimes inputList 25)

    print ans1  -- 198075