-- Advent of Code 2024, Day 11
-- reads input from input_day11.txt, prints results to console

import Data.List

numDigits :: Integer -> Integer
numDigits x = let divd = div x 10 in (if divd == 0 then 1 else 1 + numDigits divd)

splitStone :: Integer -> Integer -> [Integer]
splitStone x digs = let p = 10 ^ div digs 2 in [div x p, mod x p]

blinkSingleStone :: Integer -> [Integer]
blinkSingleStone s
    | s == 0 = [1]
    | even (numDigits s) = splitStone s digs
    | otherwise = [2024 * s]
    where digs = numDigits s

-- rather than calculating sequences directly (due to astronomical length)
-- consider that we only need the length of the resulting sequnce,
-- which isn't affected by the order of the stones - thus we'll only count
-- the number of each stones for each number
type StoneCount  = [(Integer, Integer)]     -- [(num1, count1), (num2, count2) ... ]

combineStones :: (Integer, [Integer]) -> (Integer, [Integer])
combineStones (acc, sortedStones)
    | null remainingStones = (acc + 1, [])                                    -- end of the list, no next stone
    | head sortedStones /= head remainingStones = (acc + 1, remainingStones)  -- next stone different
    | otherwise = combineStones (acc + 1, remainingStones)                    -- otherwise keep accumulating
    where remainingStones = tail sortedStones

combineAllStones :: [Integer] -> StoneCount
combineAllStones sortedStones = 
        case sortedStones of
            []  -> []
            x:_ -> let (count, rest) = combineStones (0, sortedStones)
                    in (x, count) : combineAllStones rest

stoneListToCount :: [Integer] -> StoneCount
stoneListToCount stones = combineAllStones (sort stones)

-- stoneListToCount creates a list of stone numbers and their counts
-- [(n1, c1), (n2, c2) ...] in which n1 < n2 < n3 etc (due to the sort used in its construction)
-- we can use this fact when adding two counts together
addStoneCounts :: StoneCount -> StoneCount -> StoneCount
addStoneCounts x [] = x
addStoneCounts [] y = y
addStoneCounts x y
    | fst xi == fst yi  = (fst xi, snd xi + snd yi) : addStoneCounts (tail x) (tail y)
    | fst xi < fst yi   = xi : addStoneCounts (tail x) y
    | fst xi > fst yi   = yi : addStoneCounts x (tail y)
    where xi = head x; yi = head y

multiplyStoneCount :: Integer -> StoneCount -> StoneCount
multiplyStoneCount mul = map (\(n,c) -> (n,mul*c))

totalStoneCount :: StoneCount -> Integer
totalStoneCount stones = sum (map snd stones)

-- now we can implement the actual blinking operation on the stone count
blinkSingleStoneToCount :: (Integer, Integer) -> StoneCount
blinkSingleStoneToCount (s, c) = multiplyStoneCount c (stoneListToCount (blinkSingleStone s))

blinkStoneCount :: StoneCount -> StoneCount
blinkStoneCount stones = foldl addStoneCounts [] (map blinkSingleStoneToCount stones)

blinkNStoneCount :: StoneCount -> Int -> StoneCount
blinkNStoneCount stones n =
    case n of
        0 -> stones
        1 -> blinkStoneCount stones
        _ -> blinkNStoneCount (blinkStoneCount stones) (n - 1)

main = do
    -- read and parse input into list
    inputString <- readFile "input_day11.txt"
    let inputList = map read (words (head (lines inputString)))  :: [Integer]

    let initStoneCounts = stoneListToCount inputList
    let ans1 = totalStoneCount (blinkNStoneCount initStoneCounts 25)
    let ans2 = totalStoneCount (blinkNStoneCount initStoneCounts 75)

    print ans1  -- 198075
    print ans2  -- 235571309320764
