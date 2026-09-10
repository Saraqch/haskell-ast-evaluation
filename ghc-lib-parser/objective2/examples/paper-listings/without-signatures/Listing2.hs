module Listing2 where
b (x:[]) = 1
b (x:xs) = 1 + b xs
