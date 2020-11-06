import Data.Maybe

type ABC = [(Char, Char)]
mapping :: ABC
mapping = [(toEnum currentChar::Char, toEnum nextChar::Char) | currentChar <- [97..122], let nextChar = (([100..122] ++ [48..50]) !! (currentChar - 97))] ++
          [(toEnum currentChar::Char, toEnum nextChar::Char) | currentChar <- [48..57] , let nextChar = (([51..57] ++ [65..67]) !! (currentChar - 48))] ++
          [(toEnum currentChar::Char, toEnum nextChar::Char) | currentChar <- [65..90] , let nextChar = (([68..90] ++ [97..99]) !! (currentChar - 65))]

encodeCaesar myString = map encode myString
    where
        encode x = (fromMaybe ' ' $ lookup x mapping)

decodeCaesar myString = map decode myString
    where
        decode x = findKey x mapping 
        findKey searchedObject ((key, object):rest)
            | searchedObject == object = key
            | otherwise                = findKey searchedObject rest