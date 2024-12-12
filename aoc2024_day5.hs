-- Advent of Code 2024, Day 5
-- reads input from input_day5.txt, prints results to console

import Data.List
import Data.Maybe

-- input parsing functions
replaceDelimsWithWhitespace :: [Char] -> [Char]
replaceDelimsWithWhitespace str =
    let
        repl '|' = ' '
        repl ',' = ' '
        repl chr = chr
    in  map repl str

-- rule strings are of the form "AB|CD"
ruleStrToTuple :: [Char] -> (Int, Int)
ruleStrToTuple str =
    let splitStr = words (replaceDelimsWithWhitespace str)
    in (read (head splitStr), read (head (tail splitStr)))

-- test strings are comma separated strings of two (digit) numbers
testStrToList :: String -> [Int]
testStrToList str = map read (words (replaceDelimsWithWhitespace str))

-- test logic
-- test a single rule (a, b) on a single page list, this function assumes no duplicate pages
-- (note pagesList argument comes first to allow partial application in testAllPageOrderRules below)
pageOrderRuleTest :: [Int] -> (Int, Int) -> Bool
pageOrderRuleTest pagesList (priorPage, subsequentPage)  =
    -- if either element is not present, the rule is trivially satisfied, if not priorPage must preceed subsequentPage
    case elemIndex priorPage pagesList of
        Just priorIdx -> case elemIndex subsequentPage pagesList of
                        Just subsequentIdx -> priorIdx < subsequentIdx
                        Nothing  -> True
        Nothing  -> True

-- this will test all the rules given as a list of tuples (priorPage, subsequentPage)
testAllPageOrderRules :: [(Int, Int)] -> [Int] -> Bool
testAllPageOrderRules rulesTuples testList = all (pageOrderRuleTest testList) rulesTuples

-- part 2
-- the approach to fixing a sequence will be: if a sequence fails pageOrderRuleTest for a given pair of pages, swap the offending pages
pageOrderRuleFixer :: [Int] -> (Int, Int) -> [Int]
pageOrderRuleFixer pagesList (priorPage, subsequentPage) =
    if pageOrderRuleTest pagesList (priorPage, subsequentPage)
    then pagesList
    else let
            repl x | x == priorPage = subsequentPage
                   | x == subsequentPage = priorPage
                   | otherwise = x
        in  map repl pagesList

-- to fix a sequence, iterate over all rulesTuples applying pageOrderRuleFixer for each
fixPageOrderRulesPass :: [(Int, Int)] -> [Int] -> [Int]
fixPageOrderRulesPass rulesTuples fixedListAcc =
    case rulesTuples of
        [] -> fixedListAcc
        _  -> fixPageOrderRulesPass (tail rulesTuples) (pageOrderRuleFixer fixedListAcc (head rulesTuples))

-- as one pass of fixPageOrderRulesPass function doesn't guarantee all rules satisfied,
-- we'll run it repeatedly until it does
fixAllPageOrderRules :: [(Int, Int)] -> [Int] -> [Int]
fixAllPageOrderRules rulesTuples pagesList =
    let singlePassResult = fixPageOrderRulesPass rulesTuples pagesList in
        if testAllPageOrderRules rulesTuples singlePassResult then 
            singlePassResult
        else fixAllPageOrderRules rulesTuples singlePassResult


main = do
    -- read in input file
    inputString <- readFile "input_day5.txt"
    let inputLines = lines inputString

    -- split rules and test lists into two lists using the whitespace line between them
    let inputLists = splitAt (fromJust (elemIndex "" inputLines)) inputLines
    let rulesStrList = fst inputLists
    let testsStrList = tail (snd inputLists)    -- first element of second list is the "" delimiter we split at


    let rulesTupleList = map ruleStrToTuple rulesStrList
    let testsListList = map testStrToList testsStrList

    -- we can now run the test
    let passingTestsList = filter (testAllPageOrderRules rulesTupleList) testsListList

    -- to get the middle page numbers, conveniently all the lists are off odd length
    let middlePageNums = map (\list -> list !! div (length list - 1) 2) passingTestsList
    let ans1 = sum middlePageNums

    -- part 2
    -- get failing page sequences, then fix them
    let failingTestsList = filter (not . testAllPageOrderRules rulesTupleList) testsListList
    let fixedTestsList = map (fixAllPageOrderRules rulesTupleList) failingTestsList

    -- get sum of middle page nums as before
    let middlePageNums2 = map (\list -> list !! div (length list - 1) 2) fixedTestsList
    let ans2 = sum middlePageNums2

    print ans1  -- 4872
    print ans2  -- 5564
    
