import Data.List
import Data.Function
import Data.Char

compress :: String -> [(Char, Int)]
compress n = [(head a, length a) | a <- group n]

decompress :: [(Char, Int)] -> String
decompress n = concat [replicate b a | (a, b) <- n]

mountain :: Integer -> String
mountain n = concat [replicate (fromIntegral a::Int) '#' ++ "\n" | a <- [1..n]]
