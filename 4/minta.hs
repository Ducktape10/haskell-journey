divides 0 0 = True
divides 0 _ = False
divides a b = mod b a == 0

isSingleton :: [a] -> Bool
isSingleton [] = False
isSingleton (x:xs) = null xs

countAttackingQueens (a, b) [] = 0
countAttackingQueens (a, b) n = length [(x, y) | (x, y) <- n , x == a || b == y || abs (a-x) == abs (b-y)]