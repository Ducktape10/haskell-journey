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

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn p l
    | isSame      = [[],[]]
    | firstSame   = [] : splitList p l
    | otherwise   = splitList p l
    where
        firstSame      = p == (head l)
        isSame         = firstSame && null (tail l)
        splitList _ [] = []
        splitList pattern list
            | isDoubleDropped = [] : splitList pattern (drop 1 xs)
            | otherwise       = x  : splitList pattern xs
            where
                (x, xs)         = break (== pattern) (dropIfPattern pattern list)
                isDropped       = head list == pattern
                isDoubleDropped = isDropped && not (null x)  && head list == head x

        dropIfPattern pattern list
            | pattern == head list = drop 1 list
            | otherwise            =        list

emptyLines [] = [1]
emptyLines t
    | otherwise = getEmptyIndex 1 0 0 t
    where
        getEmptyIndex _ _ _ [] = []
        getEmptyIndex index db cIndex (x:xs)
            | fEmpty     = index               : getEmptyIndex (index + 1) (db + 1) cIndex xs 
            | lastDEmpty = index : (index + 1) : getEmptyIndex (index + 1) 0 cIndex xs  
            | lastEmpty  =         (index + 1) : getEmptyIndex (index + 1) 0 cIndex xs
            | isEmpty    = index               : getEmptyIndex (index + 1) 0 cIndex xs
            | isBreak    =                       getEmptyIndex (index + 1) (db + 1) cIndex xs
            | otherwise  =                       getEmptyIndex index 0 (cIndex + 1) xs
            where
                last' = null xs
                isBreak = x == '\n'
                isEmpty = (isBreak && db == 1)
                fEmpty = isBreak && index == 1 && cIndex == 0
                lastEmpty = isBreak && last'
                lastDEmpty = isEmpty && last'

csv n
    | otherwise = [splitComma a| a <- splitLines]
    where
        splitLines   = splitOn '\n' n
        splitComma n = splitOn ',' n
        