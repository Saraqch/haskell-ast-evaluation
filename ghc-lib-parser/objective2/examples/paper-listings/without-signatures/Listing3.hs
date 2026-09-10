module Listing3 where
import Data.Monoid (Sum (Sum));
c n
    | n < 2 = (Sum 1, ()) *> pure 1
    | otherwise = (Sum 1, (-)) <*> c (n - 1) <*> c (n - 2)
