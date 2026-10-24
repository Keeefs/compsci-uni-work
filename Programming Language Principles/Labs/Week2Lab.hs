-- q1
-- (\x -> x + 1)
-- (\x -> x - 1)
-- (\x -> x > 1 && [y | y <- [2..x-1], x `mod` y == 0]

-- q2
indices :: Eq a => a -> [a] -> [Int]
indices x xs = let pairs = zip xs [i | i <- [0..((length xs)-1)]] in
            [snd y | y <- pairs, fst y == x]

-- q3
consecutiveSum :: Num a => [a] -> [a]
consecutiveSum [] = []
consecutiveSum (x:xs) = [y+z | (y,z) <- (zip (x:xs) xs)]

-- q4
oddSquaresC :: Integral a => [a] -> [a]
oddSquaresC xs = [x^2 | x <- xs, odd x]

oddSquaresR :: Integral a => [a] -> [a]
oddSquaresR [] = []
oddSquaresR (x:xs)
    | odd x = x^2 : oddSquaresR xs
    | otherwise = oddSquaresR xs

-- q5
factors :: Integer -> [Integer]
factors n = [x | x <- [1..n], n `mod` x == 0]

isPrime :: Integer -> Bool
isPrime n = if length (factors n) == 2 && n >= 2 then True else False

primeFactors :: Integer -> [Integer]
primeFactors n = [x | x <- drop 1 (factors n), isPrime x]

-- q6
sumR :: Num a => [a] -> a
sumR [] = 0
sumR (x:xs) = x + sumR(xs)

takeR :: Int -> [a] -> [a]
takeR _ [] = []
takeR n _ | n <= 0 = [] 
takeR n (x:xs) = x : takeR (n-1) xs

lastR :: [a] -> a
lastR [] = error "Error - no elements in provided list"
lastR (x:[]) = x 
lastR (_:xs) = lastR xs

-- q7
zipR :: [a] -> [b] -> [(a,b)]
zipR [] _ = []
zipR _ [] = []
zipR (x:xs) (y:ys) = (x,y) : zipR xs ys 

-- q8
euclid :: Int -> Int -> Int
euclid 0 x = x 
euclid x 0 = x 
euclid x y 
    | x == y && x > 0 && y > 0 = x
    | x > y = euclid (x-y) y 
    | otherwise = euclid x (y-x)

-- q9
digitsOf :: Integer -> [Integer]
digitsOf num = reverse (digitsRev num)

digitsRev x | x `div` 10 == 0 = [x]
digitsRev num = num `mod` 10 : digitsRev (num `div` 10)
