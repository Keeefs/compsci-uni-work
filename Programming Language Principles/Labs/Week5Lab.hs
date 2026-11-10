import Data.Monoid (Sum(..))
import Text.Read (readMaybe)

-- q1
-- Just 3, applied to "red"
-- Just ["cd", "ab"], applied to the list
-- [Just "ba", Nothing, Just "dc"], applied to the strings in the list

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 f x = fmap (fmap f) x
