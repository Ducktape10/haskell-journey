
import Data.List

reverse' :: [a] -> [a]
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
    | (n < 0) = take (abs (fromIntegral(n)::Int)) (x:xs)

getSampleIndices _ _ [] = []
getSampleIndices i w s
    | isPrefixOf w s = i : getSampleIndices 0 w (drop (length w) s)
    | otherwise      = getSampleIndices (i + 1) w (drop 1 s)

splitAtIndices l s []     = s : []
splitAtIndices l s (x:xs) = take x s : splitAtIndices l (drop (x + l) s) xs

attachEmpty (x:[]) = [x] : []
attachEmpty (x:xs) = [x] : attachEmpty xs

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn [] a = [] : attachEmpty a
splitOn a b  = splitAtIndices (length a) b (getSampleIndices 0 a b)



getEmptyIndex i [] = []
getEmptyIndex i (x:xs)
    | null x    = i : getEmptyIndex (i + 1) xs
    | otherwise =  getEmptyIndex (i + 1) xs

emptyLines t = getEmptyIndex 1 (splitOn "\n" t)

