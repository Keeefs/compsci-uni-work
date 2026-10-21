module Week1Lab where 
    import Data.Char (isLower, toUpper)
    -- n = a `div` length xs
    --     where 
    --         a = 10
    --         xs = [1,2,3,4,5]

    dartScoreIf x y = if d <= 1 then 10 else if 1 < d && d <= 25 then 5 else if 25 < d && d <= 100 then 1 else 0
        where
            d = x^2 + y^2

    dartScoreGuards (x, y) 
        | d <= 1 = 10
        | d > 1 && d <= 25 = 5
        | d > 25 && d <= 100 = 1
        | otherwise = 0
        where d = x^2 + y^2


    swapPair :: (a,b) -> (b,a)    
    swapPair (x,y) = (y, x)     

    safeTail :: [a] -> [a]
    safeTail [] = []
    safeTail (_ : xs) = xs

    firstAndLast :: [a] -> (a,a)
    firstAndLast (x:xs) = (x, last (x:xs))

    isSingleton :: [a] -> Bool
    isSingleton [_] = True
    isSingleton _ = False

    ticketPrice :: Int -> Double
    ticketPrice age
        | age < childAge = defaultPrice * 0.5
        | age >= elderlyAge = defaultPrice * 0.7
        | otherwise = defaultPrice
        where 
            childAge = 12
            elderlyAge = 65
            defaultPrice = 20.00 

    rotate :: Int -> [a] -> [a]
    rotate 0 xs = xs 
    rotate n (x:xs) = rotate (n - 1) xs ++ [x]

    equalPairs :: Eq a => a -> a -> a -> Int
    equalPairs x y z = (if x == y then 1 else 0) + (if x == z then 1 else 0) + (if y == z then 1 else 0)

    capitalizeStart :: String -> String
    capitalizeStart (x:xs)
        | isLower x = toUpper x : xs
        | otherwise = x:xs
