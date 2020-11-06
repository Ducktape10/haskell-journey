import Data.List

repeat' a = a : repeat' a

compress :: Eq a => [a] -> [(a, Int)]
compress a = map (\g -> (head g, length g)) (group a)

decompress :: [(a, Int)] -> [a]
decompress a = concat $ map (\(head,amount) -> replicate amount head) a

apsOnLists :: [a -> b] -> [[a]] -> [[b]]
apsOnLists a b = map (\(a,b) -> map a b) (zip a b)