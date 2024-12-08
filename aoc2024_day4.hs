-- Advent of Code 2024, Day 4
-- reads input from input_day4.txt, prints results to console

import Data.List

matchTest :: (Char, Char, Char, Char) -> Bool
matchTest (a, b, c, d) = ((a == 'X') && (b == 'M') && (c == 'A') && (d == 'S')) || ((a == 'S') && (b == 'A') && (c == 'M') && (d == 'X'))

countMatches :: [Char] -> Int
countMatches str = length (filter matchTest (zip4 str (tail str) (tail (tail str)) (tail (tail (tail str)))))

-- to count on diagonals, we'll effectively skew the input strings with an offset
-- these functions will count matches in a block of 4 lines at a time, which we can then slide down through all the data
countInLeadingDiagonalBlock :: ([Char], [Char], [Char], [Char]) -> Int
countInLeadingDiagonalBlock (a, b, c, d) = length (filter matchTest (zip4 (tail (tail (tail a))) (tail (tail b)) (tail c) d))

countInTrailingDiagonalBlock :: ([Char], [Char], [Char], [Char]) -> Int
countInTrailingDiagonalBlock (a, b, c, d) = length (filter matchTest (zip4 a (tail b) (tail (tail c)) (tail (tail (tail d)))))


main = do
    -- read and parse input into list
    inputString <- readFile "input_day4.txt"
    let inputLines = lines inputString

    -- part 1
    let horizontalMatchCount = sum (map countMatches inputLines)
    let verticalMatchCount = sum (map countMatches (transpose inputLines))

    -- to test on diagonals, we'll test 4 lines at a time, sliding that "window" down the data, testing each with countInLeadingDiagonalBlock and countInTrailingDiagonalBlock
    let diagonalTestSlidingWindow = zip4 inputLines (tail inputLines) (tail (tail inputLines)) (tail  (tail (tail inputLines)))
    let leadingDiagonalMatchCount = sum (map countInLeadingDiagonalBlock diagonalTestSlidingWindow)
    let trailingDiagonalMatchCount = sum (map countInTrailingDiagonalBlock diagonalTestSlidingWindow)
    
    let ans1 = horizontalMatchCount + verticalMatchCount + leadingDiagonalMatchCount + trailingDiagonalMatchCount
    print ans1  -- 2543