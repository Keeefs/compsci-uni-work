import Data.Char (toLower)

-- q1
data ShoppingEntry = ShoppingEntry {
    itemName :: String,
    unitPrice :: Double,
    quantity :: Int
} deriving (Eq, Show)

totalPrice :: ShoppingEntry -> Double
totalPrice entry = (unitPrice entry) * fromIntegral(quantity entry) 

buyOneMore :: ShoppingEntry -> ShoppingEntry
buyOneMore entry = entry {quantity = (quantity entry) + 1}

-- q2
newtype Month = Month Int deriving (Eq, Show)

makeMonth :: Int -> Maybe Month
makeMonth num 
    | num <= 12 && num >= 1 = Just (Month num)
    | otherwise             = Nothing 

monthNumber :: Month -> Int
monthNumber (Month n) = n 

monthEither :: Int -> Either String Month
monthEither n = 
    case makeMonth n of
        Nothing    -> Left "Invalid month"
        Just month -> Right month
    
-- q3
newtype Name = Name String deriving Show

instance Eq Name where
    Name x == Name y = 
        map toLower (x) == map toLower y

-- q4
data Number = Finite Integer | Infinite
    deriving (Eq, Show)

instance Ord Number where
    compare (Finite x) (Finite y) = compare x y   
    compare Infinite (Finite y) = GT
    compare (Finite x) Infinite = LT
    compare Infinite Infinite = EQ

-- q5
data Pastry = Croissant | Eclair | Millefeuille
    deriving Show
data Fruit = Banana Double | Apple Double
    deriving Show

class Price a where
    price :: a -> Double

instance Price Pastry where
    price Croissant = 2.50
    price Eclair = 3.00
    price Millefeuille = 5.00

instance Price Fruit where
    price (Banana kg) = kg * 1.60
    price (Apple kg) = kg * 2.50

bill :: Price a => [a] -> Double
bill = sum . map price

-- q6
data NonEmpty a = NonEmpty a [a] 
    deriving Show

first :: NonEmpty a -> a
first (NonEmpty x _) = x  

toList :: NonEmpty a -> [a]
toList (NonEmpty x xs) = x:xs

prepend :: a -> NonEmpty a -> NonEmpty a
prepend x (NonEmpty y ys) = NonEmpty x (y:ys)

instance Eq a => Eq (NonEmpty a) where
    NonEmpty x xs == NonEmpty y ys = (x == y) && (xs == ys)

-- q7
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Eq, Show)

data Direction = GoLeft | GoRight
    deriving (Eq, Show)

sampleTree :: Tree Int
sampleTree = 
    Node 8 (Node 3 Empty Empty) (Node 10 Empty Empty)

followPath :: [Direction] -> Tree a -> Maybe a
followPath _ Empty = Nothing
followPath [] (Node current _ _) = Just current

followPath (GoLeft : restDirections) (Node _ left _) = 
    followPath restDirections left 

followPath (GoRight : restDirections) (Node _ _ right) = 
    followPath restDirections right

-- q8
data Color = Red | Green | Blue | Mix Color Color | Invert Color
    deriving Show

type RGB = (Double, Double, Double)

toRGB :: Color -> RGB
toRGB Red = (1, 0, 0)
toRGB Green = (0, 1, 0)
toRGB Blue = (0, 0, 1)

toRGB (Mix col1 col2) = 
    let (r1, g1, b1) = toRGB col1
        (r2, g2, b2) = toRGB col2
    in 
        ((r1 + r2) / 2, (g1 + g2) / 2, (b1 + b2) / 2)

toRGB (Invert col) = 
    let (red, green, blue) = toRGB col
    in
        ((1 - red), (1- green), (1 - blue))