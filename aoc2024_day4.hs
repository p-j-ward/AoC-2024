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

-- part 2
-- we'll define our match test function to look at the X as follows:
--   tl-tr
--   --m--
--   bl-br
matchTestPart2 :: (Char, Char, Char, Char, Char) -> Bool
matchTestPart2 (tl, tr, m, bl, br) = (m == 'A')                                                             -- for a match, middle must always be an A
                                        && (((tl == 'M') && (br == 'S')) || ((tl == 'S') && (br == 'M')))   -- match on leading diagonal
                                        && (((tr == 'M') && (bl == 'S')) || ((tr == 'S') && (bl == 'M')))   -- match on trailing diagonal

-- we'll iterate with a 'window' in the same way we counted the diagonals in part 1
countPart2MatchesInBlock :: ([Char], [Char], [Char]) -> Int
countPart2MatchesInBlock (line1, line2, line3) = length (filter matchTestPart2 (zip5 line1 (tail (tail line1)) (tail line2) line3 (tail (tail line3))))


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

    -- part 2
    -- as per diagonals in part 1, now "window" is 3 lines at a time
    let part2TestSlidingWindow = zip3 inputLines (tail inputLines) (tail (tail inputLines))
    let ans2 = sum (map countPart2MatchesInBlock part2TestSlidingWindow)

    
    print ans1  -- 2543
    print ans2  -- 1930
