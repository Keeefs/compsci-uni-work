-- q1
-- map (take 3) :: [[a]] -> [[a]]

-- filter (elem 'a') :: [String] -> [String]

-- map (uncurry (+)) :: [(a,a)] -> [a]

-- zipWith ($) :: [a -> b] -> [a] -> [b]

-- (\x -> 10 / x)
-- (\x -> x / 10)
-- (\x -> 1 - x)
-- (-1) is not a section that subtracts 1 as it represents the literal value negative 1

-- q2
mapAdd3 :: [Int] -> [Int]
-- map' [] = []
-- map' x:xs = (x + 3) : map' xs
mapAdd3 xs = [x+3 | x <- xs]

filterGreaterThan7 :: [Int] -> [Int]
filterGreaterThan7 xs = [x | x <- xs, x >7]

concatPairsList :: [a] -> [a] -> [(a,a)]
concatPairsList xs ys = [(x,y) | x <- xs, y <- ys]

filterGreaterThan3Pairs pairs = [x+y | (x,y) <- pairs, x+y>3] 