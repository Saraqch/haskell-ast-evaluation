module Listing1 where
a :: Int -> Int
a x 
    | x `mod` 3 == 0 && x `mod` 5 == 0 = x ^ 2 ^ 4
    | x `mod` 3 == 0 = 5 * 8 `div` 15
    | x `mod` 5 == 0 = 5 + 8 `div` 2
    | otherwise = x - 5 - 3