-- Advent of Code 2024, Day 6

-- list zipper functions
zipperSelect :: ([a], a, [a]) -> a
zipperSelect (zl, v, zr) = v

zipperSet :: ([a], a, [a]) -> a -> ([a], a, [a])
zipperSet (zl, v, zr) newVal = (zl, newVal, zr)

zipperLeft :: ([a], a, [a]) -> ([a], a, [a])
zipperLeft (zl, v, zr) = (tail zl, head zl, v : zr)

zipperRight :: ([a], a, [a]) -> ([a], a, [a])
zipperRight (zl, v, zr) = (v : zl, head zr, tail zr)

zipperToList :: ([a], a, [a]) -> [a]
zipperToList (zl, v, zr) = reverse zl ++ (v : zr)

