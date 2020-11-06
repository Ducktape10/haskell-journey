import Data.Char

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _       = True



onAxis :: (Num a) => a -> a -> Bool
onAxis 0 _ = True
onAxis _ 0 = True
onAxis _ _ = False

punctuation :: Char -> Bool
punctuation '.' = True
punctuation '!' = True
punctuation '?' = True
punctuation _ = False

toUpperThird :: [Char] -> String
toUpperThird (x:y:z:xs) = x : y : (toUpper z) : xs
toUpperThird a = a