lucas 0 = 3
lucas 1 = 1
lucas n = lucas (n - 1) + lucas (n - 2)

longerThan :: Integral i => [a] -> i -> Bool
longerThan [] n     = (n < 0)
longerThan _  0     = True
longerThan (x:xs) n      =  (n < 0) || longerThan xs (n-1)

format :: Integral a => a -> String -> String
format 0 t      = t
format n []     = concat (take (fromIntegral(n)::Int) [" " | i <- [1..]])
format n (x:xs) = x : format (n-1) xs