module Time where

import Data.List
import Data.Char

data Time = T Int Int
    deriving (Eq)

t :: Int -> Int -> Time
t h m
    | isValid   = (T h m) :: Time
    | otherwise = error "Nem valós idő"
    where
       isValid = elem h [0..23] && elem m [0..59]

instance Show Time where
    show (T h m) =  show h ++ ":" ++ show m

isEarlier :: Time -> Time -> Bool
isEarlier (T h1 m1) (T h2 m2)
    | isHourSame    = m1 <  m2
    | otherwise     = isHourEarlier
    where
        isHourEarlier = h1 <  h2
        isHourSame    = h1 == h2

isBetween :: Time -> Time -> Time -> Bool
isBetween t1 t2 t3 = (isEarlier t1 t2 && isEarlier t2 t3) || (isEarlier t2 t1 && isEarlier t3 t2)

data Period = AM | PM
    deriving (Eq)

data USTime = US Period Int Int
    deriving (Eq)

ustime :: Period -> Int -> Int -> USTime
ustime (AM) h m
    | elem h [1..12] && elem m [0..59] = US AM h m
    | otherwise = error "Nem valós US idő"
ustime (PM) h m
    | elem h [1..12] && elem m [0..59] = US PM h m
    | otherwise = error "Nem valós US idő"

instance Show USTime where
    show (US AM h m) = "AM " ++ show h ++ ":" ++ show m
    show (US PM h m) = "PM " ++ show h ++ ":" ++ show m

ustimeToTime :: USTime -> Time
ustimeToTime (US AM 12 m) = t 0 m
ustimeToTime (US AM h m)  = t h m
ustimeToTime (US PM 12 m) = t 12 m
ustimeToTime (US PM h m)  = t (h + 12) m

timeToUSTime :: Time -> USTime
timeToUSTime (T 12 m) = ustime (PM) 12 m
timeToUSTime (T 0 m)  = ustime (AM) 12 m
timeToUSTime (T h m)
    | isAM      = ustime (AM) h m
    | otherwise = ustime (PM) (h - 12) m
    where
        isAM = h < 12