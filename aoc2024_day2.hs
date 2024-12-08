-- Advent of Code 2024, Day 2
-- reads input from input_day2.txt, prints results to console

main = do
    -- read and parse input into list of lists
    inputString <- readFile "input_day2.txt"
    let inputLists = map (map read . words) (lines inputString) :: [[Int]]

    -- part 1: a report list is considered safe if:
    -- 1) all differences are of the same sign and
    -- 2) the differences are of magnitude 1, 2, or 3
    let reportIsSafeHelper (x, y, z) = (x /= y) && (abs (x - y) <= 3) && (y /= z) && (abs (y - z) <= 3) && (signum (y - x) == signum (z - y))
    let reportIsSafe report = all reportIsSafeHelper (zip3 report (tail report) (tail (tail report)))
    let ans1 = length (filter reportIsSafe inputLists)

    -- part 2: let's just use brute force
    let unsafeReports = filter (not . reportIsSafe) inputLists
    let problemDampenerSolvedReports = length (filter (any reportIsSafe) (map removeEachElement unsafeReports))
    let ans2 = ans1 + problemDampenerSolvedReports

    print ans1  -- 479
    print ans2  -- 531


-- functions for part 2
removeNthElement :: Int -> [a] -> [a]
removeNthElement n list = 
      case n of 
          0 -> tail list
          _ -> head list: removeNthElement (n-1) (tail list)

removeEachElementHelper :: Int -> [Int] -> [[Int]] -> [[Int]]
removeEachElementHelper n originalList newList =
    case n of 
        0 -> removeNthElement 0 originalList : newList
        _ -> removeEachElementHelper (n - 1) originalList (removeNthElement n originalList : newList)

removeEachElement :: [Int] -> [[Int]]
removeEachElement list = removeEachElementHelper (length list - 1) list []