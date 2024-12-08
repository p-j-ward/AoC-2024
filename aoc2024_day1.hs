-- Advent of Code 2024, Day 1
-- reads input from input_day1.txt, prints results to console

import Data.List

main = do
    -- read and parse input into list
    inputString <- readFile "input_day1.txt"
    let inputPairs = map (map read . words) (lines inputString) :: [[Int]]

    -- sort the two lists
    let leftList = sort (map head inputPairs)
    let rightList = sort (map last inputPairs)

    -- part1: we want the sum total of the (magnitude of the) elementwise differences
    let diffs = map (\x -> (fst x) - (snd x)) (zip leftList rightList)
    let ans1 = sum (map abs diffs)

    -- part2: similarity score, the sum total of - each number in the left list, multiplied by the number of times it appears in the right list
    let similarity_score_for_element x = x * (length (filter (== x) rightList))
    let ans2 = sum (map similarity_score_for_element leftList)

    print ans1  -- 2970687
    print ans2  -- 23963899
