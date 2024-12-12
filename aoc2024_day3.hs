-- Advent of Code 2024, Day 3
-- reads input from input_day3.txt, prints results to console

import Data.Char

-- we want to match mul(%i,%i), if such a string 
matchStateMachine :: Int -> [Char] -> Int -> Int -> Maybe (Int, Int)
matchStateMachine stateNum str firstIntAcc secondIntAcc  = 
    case stateNum of
        0 -> if head str == 'm' then matchStateMachine 1 (tail str) 0 0 else Nothing
        1 -> if head str == 'u' then matchStateMachine 2 (tail str) 0 0 else Nothing
        2 -> if head str == 'l' then matchStateMachine 3 (tail str) 0 0 else Nothing
        3 -> if head str == '(' then matchStateMachine 4 (tail str) 0 0 else Nothing
        -- match at least one digit for a valid string
        4 -> if isDigit (head str) then matchStateMachine 5 (tail str) (digitToInt (head str)) 0  else Nothing
        -- match the rest of the digits and accumulate them into firstInt, if there are any
        5 -> if isDigit (head str) then 
            matchStateMachine 5 (tail str) ((firstIntAcc * 10) + digitToInt (head str)) 0 
        else if head str == ',' then matchStateMachine 6 (tail str) firstIntAcc 0 else Nothing
        -- match second int, needs at least one digit
        6 -> if isDigit (head str) then matchStateMachine 7 (tail str) firstIntAcc (digitToInt (head str))  else Nothing
        -- match the rest of the digits of the second int, if there are any
        7 -> if isDigit (head str) then matchStateMachine 7 (tail str) firstIntAcc ((secondIntAcc * 10) + digitToInt (head str)) 
            else if head str == ')' then 
                Just (firstIntAcc, secondIntAcc)
            else Nothing

-- sum over a list, whever we get a valid mul(%i,%i) string we accumulate it
sumValidMatches :: [Char] -> Int -> Int
sumValidMatches str acc =
    if null str then
        acc
    else
        case matchStateMachine 0 str 0 0 of
            Just (x,y) -> sumValidMatches (tail str) (acc + (x*y))
            Nothing    -> sumValidMatches (tail str) acc

main = do
    -- read input
    inputString <- readFile "input_day3.txt"

    let ans1 = sumValidMatches inputString 0
    print ans1