-- Advent of Code 2024, Day 6

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
