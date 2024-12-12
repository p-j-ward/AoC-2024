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
        [] -> Nothing -- right list is empty, can't go left any further
        x:xs -> Just (v:zl, x, xs)

zipperToList :: ZipperList a -> [a]
zipperToList (zl, v, zr) = reverse zl ++ (v : zr)

-- 2D list zipper functions
zipper2Select :: ([ZipperList a], ZipperList a, [ZipperList a]) -> a
zipper2Select (zl, v, zr) = zipperSelect v

zipper2Set :: ZipperList (ZipperList a) -> a -> ZipperList (ZipperList a)
zipper2Set (zl, v, zr) newVal = (zl, zipperSet v newVal, zr)

zipper2Up :: ZipperList (ZipperList a) -> Maybe (ZipperList (ZipperList a))
zipper2Up = zipperLeft

zipper2Down :: ZipperList (ZipperList a) -> Maybe (ZipperList (ZipperList a))
zipper2Down = zipperRight

zipper2Left :: ZipperList (ZipperList a) -> Maybe (ZipperList (ZipperList a))
zipper2Left (zl, v, zr) = do
    newZl <- traverse zipperLeft zl
    newV  <- zipperLeft v
    newZr <- traverse zipperLeft zr
    return (newZl, newV, newZr)

zipper2Right :: ZipperList (ZipperList a) -> Maybe (ZipperList (ZipperList a))
zipper2Right (zl, v, zr) = do
    newZl <- traverse zipperRight zl
    newV  <- zipperRight v
    newZr <- traverse zipperRight zr
    return (newZl, newV, newZr)

zipper2ToList :: ZipperList (ZipperList a) -> [[a]]
zipper2ToList z = map zipperToList (zipperToList z)
