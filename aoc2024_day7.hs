import Data.Bits

createOpList :: Int -> [Int -> Int -> Int]
createOpList n =  [(\x -> if x == 0 then (+) else (*)) ( n .&. (2^i)) | i <- [0..]]

tryEqn ::  Int -> [Int] -> Int -> Bool
tryEqn result eqn n = result == foldl (\acc (x,newOp) -> newOp acc x) (head eqn) (zip (tail eqn) (createOpList n))

eqnSatisfiable :: (Int, [Int]) -> Bool
eqnSatisfiable (result, eqns) = any (tryEqn result eqns) [1..2^length eqns]

-- input to this function is of the form [(target val, [eqn nums]), ...]
sumValidCalibrations :: [(Int, [Int])] -> Int
sumValidCalibrations testList = sum (map fst (filter eqnSatisfiable testList))


main = do
    -- read and parse input
    inputString <- readFile "input_day7.txt"
    let inputNoColons = let rep ':' = ' '; rep c = c in  map rep inputString
    let inputData = map (map read . words) (lines inputNoColons) :: [[Int]]

    -- calculate sum of target values for satisfiable eqns
    let ans1 = sumValidCalibrations (map (\elem -> (head elem, tail elem)) inputData)

    print ans1  -- 267566105056