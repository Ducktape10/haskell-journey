reszesedes :: Int -> Int
reszesedes n = ((n*4)::Int) `div` (2::Int)

nezoszam = 2645439
minosszeg = 10000000

profit :: Bool
profit = minosszeg < (nezoszam * 6)/2

nemVesztesegesAr :: Double
nemVesztesegesAr = minosszeg/nezoszam*2