toString :: Int -> String
toString 0 = ""
toString n
    | mod n 10 == 0 = toString (div n 10) ++ "0"
    | mod n 10 == 1 = toString (div n 10) ++ "1"
    | mod n 10 == 2 = toString (div n 10) ++ "2"
    | mod n 10 == 3 = toString (div n 10) ++ "3"
    | mod n 10 == 4 = toString (div n 10) ++ "4"
    | mod n 10 == 5 = toString (div n 10) ++ "5"
    | mod n 10 == 6 = toString (div n 10) ++ "6"
    | mod n 10 == 7 = toString (div n 10) ++ "7"
    | mod n 10 == 8 = toString (div n 10) ++ "8"
    | mod n 10 == 9 = toString (div n 10) ++ "9"

faculty :: Int -> Int
faculty 0 = 1
faculty n = n * faculty (n-1)

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

greater :: Int -> Int -> Bool
greater n1 n2
    | n1 > n2   = True
    | otherwise = False

less :: Int -> Int -> Bool
less n1 n2
    | n1 < n2   = True
    | otherwise = False

firstelement :: [a] -> a
firstelement (x:xs) = x

lastelement :: [a] -> a
lastelement [x] = x
lastelement (x:xs) = last xs

sliceoutlast :: [a] -> [a]
sliceoutlast []     = error "no element in list"
sliceoutlast [x] = []
sliceoutlast (x:xs) = x:sliceoutlast xs

sliceoutfirst :: [a] -> [a]
sliceoutfirst (x:xs) = xs

sliceoutfirstandlast :: [a] -> [a]
sliceoutfirstandlast [] = error "no element in list"
sliceoutfirstandlast l  = sliceoutlast (sliceoutfirst l)

palindrome :: Eq a => [a] -> Bool
palindrome [x] = True
palindrome [] = True
palindrome l
    | firstelement l == lastelement l = palindrome (sliceoutfirstandlast l)
    | otherwise = False

total :: [Int] -> Int
total = sum

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + length xs

-- range :: Int -> Int -> Int -> [Int]
-- range start stop 0 = error "0 steps means infinite range"
-- range start stop steps
--     | start > stop-1 = []
--     | steps < 0 = [start] ++ range (start + steps) stop steps
--     | steps > 0 = 