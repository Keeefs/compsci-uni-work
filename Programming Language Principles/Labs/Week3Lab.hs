import Data.Char (isSpace)
import Data.Char (toLower)
import Data.List (sort)

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


filterGreaterThan3Pairs :: (Ord a, Num a) => [(a, a)] -> [a]
filterGreaterThan3Pairs pairs = [x+y | (x,y) <- pairs, x+y>3] 

q2b1 :: (Ord a, Num a) => [(a,a)] -> [a]
q2b1 = filter (\x -> x > 10) . map (\pair -> fst pair + snd pair)  

q2b2 :: (Ord a, Num a) => [(a,a)] -> [a]
q2b2 = map (\pair -> fst pair + snd pair) . filter (\pair -> fst pair > 10)

-- q3
-- isSpace :: Char -> Bool
-- words   :: String -> [String]
-- unwords :: [String] -> String

countBlank :: [String] -> Int
countBlank xs = length (filter (all isSpace) xs)

removeLongWords :: Int -> String -> String
removeLongWords n text = unwords (filter (\word -> (length word) <= n) (words text))

countBlank' :: [String] -> Int
countBlank' = length . filter (all isSpace)

removeLongWords' :: Int -> String -> String
removeLongWords' n = unwords . filter (\word -> (length word) <= n) . words

-- q4
-- (2 - (5 - (8 - 0))) = 5
-- (((0 - 2) - 5) - 8) = -15
anyF :: Eq a => a -> [a] -> Bool
anyF x ys = foldr (\y found -> x == y || found) False ys

filterF :: (a -> Bool) -> [a] -> [a]
filterF f = foldr step []
    where
        step x ys
            | f x = x:ys
            | otherwise = ys

reverseF :: [a] -> [a]
reverseF = foldl (\acc x -> x : acc) []

maximumBad :: [Int] -> Int
maximumBad = foldr max 0
-- [-1,-2] As the first value to compare to is 0 (despite not being apart of the actual input), negative numbers will not work properly 

maximumF :: [Int] -> Int
maximumF (x:xs) = foldr max x xs

-- q5
fromDigits :: [Int] -> Int
fromDigits = foldl (\acc x -> 10 * acc + x) 0
-- The accumulator represents the number produced from the digits consumed so far

fromBase :: Int -> [Int] -> Int
fromBase n = foldl (\acc x -> n * acc + x) 0

-- q6
anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter (\otherWord -> anagramCheck lowerWord sortedWord otherWord)  
    where
        lowerWord = map toLower word
        sortedWord = sort lowerWord
        
anagramCheck :: String -> String -> String -> Bool
anagramCheck lowerWord sortedWord otherWord = sortedWord == sortedOtherWord && lowerWord /= (map toLower otherWord)
    where 
        sortedOtherWord = sort (map toLower otherWord)

-- q7
-- altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
-- altMap f1  f2 [] = []
-- altMap f1 f2 (x:xs) = (altMapHelper f1 f2 x (length xs)) : altMap f1 f2 xs

-- altMapHelper f1 f2 x len
--     | len `mod` 2 == 0 = f1 x
--     | otherwise = f2 x 

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1  f2 [] = []
altMap f1 f2 (x:xs) = (f1 x) : (altMap f2 f1 xs)

luhnDouble :: Int -> Int
luhnDouble num = let x = num*2 
    in if x > 9 then x - 9 else x 

luhn :: [Int] -> Bool
luhn [] = False
luhn xs = let value = sum (altMap id luhnDouble (reverse xs)) in
    value `mod` 10 == 0 

-- q8
composeAll  :: [a -> a] -> a -> a
composeAll fs x = (foldr (\f acc -> (f . acc)) id fs) x

pipeline :: [a -> a] -> [a] -> [a]
pipeline fs xs = map composeAll xs 