import Data.List

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = concat(reverse xs : [x] : [])

(!!!) :: [a] -> Int -> a
(!!!) t n
    | (n >= 0) = head (drop n t)
    | (n < 0)  = head (drop ((abs n) - 1) (reverse' t))

format :: Integral a => a -> String -> String
format 0 t      = t
format n []     = ' ' : format (n-1) []
format n (x:xs)
    | (n > 0) = x : format (n-1) xs
    | (n < 0) = (x:xs)

isHead a [] = False
isHead a b  = head b == a




splitOn p []     = []
splitOn p l
    | null (tail l) && p == head l = [[],[]] 
    | otherwise = splitAtIndices p l (elemIndices p l)























