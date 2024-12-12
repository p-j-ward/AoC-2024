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

-- part 2
matchDoStateMachine :: Int -> [Char] -> Bool
matchDoStateMachine stateNum str = 
    case stateNum of
        0 -> (head str == 'd') && matchDoStateMachine 1 (tail str)
        1 -> (head str == 'o') && matchDoStateMachine 2 (tail str)
        2 -> (head str == '(') && matchDoStateMachine 3 (tail str)
        3 -> head str == ')'

matchDontStateMachine :: Int -> [Char] -> Bool
matchDontStateMachine stateNum str = 
    case stateNum of
        0 -> (head str == 'd') && matchDontStateMachine 1 (tail str)
        1 -> (head str == 'o') && matchDontStateMachine 2 (tail str)
        2 -> (head str == 'n') && matchDontStateMachine 3 (tail str)
        3 -> (head str == '\'') && matchDontStateMachine 4 (tail str)
        4 -> (head str == 't') && matchDontStateMachine 5 (tail str)
        5 -> (head str == '(') && matchDontStateMachine 6 (tail str)
        6 -> head str == ')'

sumMatchesWithEnable :: [Char] -> Int -> Bool -> Int
sumMatchesWithEnable str acc mulEnabled
    | null str = acc
    | mulEnabled && matchDontStateMachine 0 str = sumMatchesWithEnable (tail str) acc False   -- disable mul
    | mulEnabled =
            case matchStateMachine 0 str 0 0 of
                Just (x,y) -> sumMatchesWithEnable (tail str) (acc + (x*y)) True
                Nothing    -> sumMatchesWithEnable (tail str) acc True
    | otherwise =
            -- if !mulEnabled, see if the next instruction matches a do()
            sumMatchesWithEnable (tail str) acc (matchDoStateMachine 0 str)

main = do
    -- read input
    inputString <- readFile "input_day3.txt"

    let ans1 = sumValidMatches inputString 0
    let ans2 = sumMatchesWithEnable inputString 0 True
    print ans1  -- 159892596
    print ans2  -- 92626942
    
