module Week1Lab where 
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