import Data.Monoid (Sum(..))
import Text.Read (readMaybe)

-- q1
-- Just 3, applied to "red"
-- Just ["cd", "ab"], applied to the list
-- [Just "ba", Nothing, Just "dc"], applied to the strings in the list

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f x = fmap (fmap f) x

-- q2
data TwoList a = 
    TwoEmpty
    | TwoNode a a (TwoList a)
    deriving (Eq, Show)

instance Functor TwoList where
    fmap h TwoEmpty = TwoEmpty
    fmap h (TwoNode x y rest) = TwoNode (h x) (h y) (fmap h rest)  


-- q3
operation :: String -> Maybe (Int -> Int)
operation "double" = Just (*2)
operation "negate" = Just negate
operation _ = Nothing

calculator :: String -> String -> Maybe Int
calculator op num = operation op <*> (readMaybe num) 

-- q4
statements :: [String] -> [String] -> [String]
statements subjects descriptions = (\x y z -> x ++ y ++ z) <$> subjects <*> [" is ", " is not "] <*> descriptions 

-- q5
data Priced a = Priced Int a deriving (Eq, Show)

instance Functor Priced where
    fmap f (Priced cost a) = Priced cost (f a)  

instance Applicative Priced where
    pure x = Priced 0 x
    Priced cost1 f1 <*> Priced cost2 f2 = Priced (cost1 + cost2) (f1 f2) 

-- q6
pairs :: TwoList Int
pairs = TwoNode 1 2 (TwoNode 3 4 TwoEmpty)

instance Foldable TwoList where
    foldr f z TwoEmpty = z
    foldr f z (TwoNode x y rest) = f x ( f y (foldr f z rest))

count :: (Foldable t, Eq a) => a -> t a -> Int
count target elems = foldr (\elem acc -> if elem == target then acc + 1 else acc) 0 elems

total pairs = getSum(foldMap Sum pairs)

-- q7
newtype Month = Month Int deriving (Eq, Show)

makeMonth :: Int -> Maybe Month
makeMonth num 
    | num <= 12 && num >= 1 = Just (Month num)
    | otherwise             = Nothing 

validateMonths :: [Int] -> Maybe [Month]
validateMonths = traverse makeMonth

validateMonthsReplaced :: [Int] -> Maybe [Month]
validateMonthsReplaced months = sequenceA (map makeMonth months) 

-- q8
instance Traversable TwoList where
    traverse _ TwoEmpty = pure TwoEmpty
    traverse f (TwoNode x y rest) = TwoNode <$> (f x) <*> (f y) <*> (traverse f rest) 

pricedLength :: String -> Priced Int
pricedLength s = Priced (length s) (length s)
