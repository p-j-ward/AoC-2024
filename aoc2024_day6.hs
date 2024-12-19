-- Advent of Code 2024, Day 6
-- reads input from input_day6.txt, prints results to console

import Data.Maybe

data GuardDirection = UpDir | RightDir | DownDir | LeftDir

rotateGuard :: GuardDirection -> GuardDirection
rotateGuard dir =
    case dir of
        UpDir    -> RightDir
        RightDir -> DownDir
        DownDir  -> LeftDir
        LeftDir  -> UpDir

directionAsFunc :: GuardDirection -> ZipperList2 Char -> Maybe (ZipperList2 Char)
directionAsFunc UpDir    = zipper2Up
directionAsFunc RightDir = zipper2Right
directionAsFunc DownDir  = zipper2Down
directionAsFunc LeftDir  = zipper2Left

guardStep :: (GuardDirection, ZipperList2 Char, Int) -> (GuardDirection, Maybe (ZipperList2 Char), Int)
guardStep (dir, z, crossCount) = 
    let newPos = directionAsFunc dir z in
        case newPos of
            Nothing -> (dir, Nothing, crossCount)   -- next position will not be valid, if guard stepped out of bounds
            Just validPos -> if '#' == zipper2Select validPos then 
                                (rotateGuard dir, Just z, crossCount)   -- if next newPos attempt is an obstacle, turn in place
                             else if 'X' /= zipper2Select validPos then
                                (dir, Just (zipper2Set validPos 'X'), crossCount + 1)
                             else (dir, Just (zipper2Set validPos 'X'), crossCount)

runGuard :: (GuardDirection, Maybe(ZipperList2 Char), Int) -> Int
runGuard (dir, z, crossCount) = 
    case z of
        Nothing     -> crossCount
        Just zValid -> runGuard (guardStep (dir, zValid, crossCount))

main = do
    inputString <- readFile "input_day6.txt"
    let inputData = lines inputString :: [[Char]]
    let initZipper = list2ToZipper inputData

    -- we'll cheat by knowing the inital guard position is line 44, col 53
    let line44 = let down 1 zd = fromJust $ zipper2Down zd
                     down nb zd = down (nb-1) (fromJust $ zipper2Down zd)
                 in down 43 initZipper
    let findGuard = let across 1 za = fromJust $ zipper2Right za
                        across na za = across (na-1) (fromJust $ zipper2Right za)
                    in across 52 line44

    -- initialise the guard, crossing out the starting square (and counting that),
    -- and run until out of bounds
    let ans1 = runGuard (UpDir, Just (zipper2Set findGuard 'X'), 1)

    print ans1  -- 5564


-- 1D list zipper functions
type ZipperList a = ([a], a, [a])

zipperSelect :: ZipperList a -> a
zipperSelect (zl, v, zr) = v

zipperSet :: ZipperList a -> a -> ZipperList a
zipperSet (zl, v, zr) newVal = (zl, newVal, zr)

zipperLeft :: ZipperList a -> Maybe (ZipperList a)
zipperLeft (zl, v, zr) =
    case zl of
        [] -> Nothing -- left list is empty, can't go left any further
        x:xs -> Just (xs, x, v:zr)

zipperRight :: ZipperList a -> Maybe (ZipperList a)
zipperRight (zl, v, zr) =
    case zr of
        [] -> Nothing -- right list is empty, can't go right any further
        x:xs -> Just (v:zl, x, xs)

zipperToList :: ZipperList a -> [a]
zipperToList (zl, v, zr) = reverse zl ++ (v : zr)

listToZipper :: [a] -> ZipperList a
listToZipper lst = ([], head lst, tail lst)

-- 2D list zipper functions
type ZipperList2 a = ZipperList (ZipperList a)

zipper2Select :: ZipperList2 a -> a
zipper2Select (zl, v, zr) = zipperSelect v

zipper2Set :: ZipperList2 a -> a -> ZipperList2 a
zipper2Set (zl, v, zr) newVal = (zl, zipperSet v newVal, zr)

zipper2Up :: ZipperList2 a -> Maybe (ZipperList2 a)
zipper2Up = zipperLeft

zipper2Down :: ZipperList2 a -> Maybe (ZipperList2 a)
zipper2Down = zipperRight

zipper2Left :: ZipperList2 a -> Maybe (ZipperList2 a)
zipper2Left (zl, v, zr) = do
    newZl <- traverse zipperLeft zl
    newV  <- zipperLeft v
    newZr <- traverse zipperLeft zr
    return (newZl, newV, newZr)

zipper2Right :: ZipperList2 a -> Maybe (ZipperList2 a)
zipper2Right (zl, v, zr) = do
    newZl <- traverse zipperRight zl
    newV  <- zipperRight v
    newZr <- traverse zipperRight zr
    return (newZl, newV, newZr)

zipper2ToList :: ZipperList2 a -> [[a]]
zipper2ToList z = map zipperToList (zipperToList z)

list2ToZipper :: [[a]] -> ZipperList2 a
list2ToZipper lst2 = ([], listToZipper (head lst2), map listToZipper (tail lst2))
