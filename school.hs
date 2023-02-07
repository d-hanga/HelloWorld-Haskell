produkt :: Int -> Int -> Int
produkt x y = x * y


faculty :: Int -> Int
faculty 0 = 1
faculty n = faculty (n-1) *n


fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)


crosssum :: Int -> Int
crosssum 0 = 0
crosssum x
    | x < 0 = - crosssum (-x)
    | otherwise = mod x 10 + crosssum (div x 10)


firstelement :: [a] -> a
firstelement [] = error "ss"
firstelement (x:xs) = x


lastelement :: [a] -> a
lastelement [] = error "ss"
lastelement [x] = x
lastelement (x:xs) = last xs


sumlist :: [Int] -> Int
sumlist [] = 0
sumlist [x] = x
sumlist (x:xs) = sumlist xs + x


len :: [a] -> Int
len [] = 0
len [x] = 1
len (x:xs) = len xs + 1


lenisn :: Int -> [a] -> Bool
lenisn 0 [] = True
lenisn 0 _ = False
lenisn _ [] = False
lenisn n (x:xs) = lenisn (n-1) xs


slicelast :: [a] -> [a]
slicelast [x] = []
slicelast (x:xs) = x:slicelast xs


slicefirstandlast :: [a] -> [a]
slicefirstandlast (x:xs) = slicelast xs


palindrom :: [Char] -> Bool
palindorm [] = True
palindrom [c] = True
palindrom x
    | lastelement x == firstelement x = palindrom (slicefirstandlast x)
    | otherwise = False


minimumelement :: [Int] -> Int
minimumelement [] = error "No element in the list!"
minimumelement [x] = x
minimumelement (x:y:ys)
    | y < x = minimumelement (y:ys)
    | otherwise = minimumelement (x:ys)


maximumelement :: [Int] -> Int
maximumelement [] = error "No element in the list!"
maximumelement [x] = x
maximumelement (x:y:ys)
    | x < y = maximumelement (y:ys)
    | otherwise = maximumelement (x:ys)


remove :: (Eq a) => [a] -> a -> [a]
remove [] v = error "Element not found!"
remove (x:xs) v
    | x == v = xs
    | otherwise = x:remove xs v


-- to[Char] :: Int -> [Char]
-- to[Char] n
--     | first == 0 = "0" ++ to[Char] (div n 10)
--     | first == 1 = "1" ++ to[Char] (div n 10)
--     | first == 2 = "2" ++ to[Char] (div n 10)
--     | first == 3 = "3" ++ to[Char] (div n 10)
--     | first == 4 = "4" ++ to[Char] (div n 10)
--     | first == 5 = "5" ++ to[Char] (div n 10)
--     | first == 6 = "6" ++ to[Char] (div n 10)
--     | first == 7 = "7" ++ to[Char] (div n 10)
--     | first == 8 = "8" ++ to[Char] (div n 10)
--     | first == 9 = "9" ++ to[Char] (div n 10)
--     where first = mod n 10


getelement :: [a] -> Int -> a
getelement [] _ = error "index out of range"
getelement (x:xs) 0 = x
getelement (x:xs) y = getelement xs (y - 1)


selectionsort :: [Int] -> [Int]
selectionsort [] = []
selectionsort xs = smallestelement:selectionsort (remove xs smallestelement)
    where smallestelement = minimumelement xs


sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [x] = True
sorted (x:y:ys)
    | x <= y = sorted (y:ys)
    | otherwise = False



_bubblesort :: Ord a => [a] -> [a]
_bubblesort [] = []
_bubblesort (x:y:ys)
    | x <= y = x:_bubblesort (y:ys)
    | otherwise = y:x:ys

bubblesort :: Ord a => [a] -> [a]
bubblesort [] = []
bubblesort [x] = [x]
bubblesort xs
    | sorted result = result
    | otherwise = bubblesort result
    where result = _bubblesort xs


sortinsert :: Ord a => [a] -> a -> [a]
sortinsert [] z = [z]
sortinsert [x] z
    | x <= z = [x, z]
    | otherwise = [z, x]
sortinsert (x:y:ys) z
    | x >= z = z:x:y:ys
    | x <= z && z <= y = x:z:y:ys
    | otherwise = x:sortinsert (y:ys) z

_insertionsort :: Ord a => [a] -> [a] -> [a]
_insertionsort xs final = foldl sortinsert final xs

insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort xs = _insertionsort xs []


filterlist :: (a -> Bool) -> [a] -> [a]
filterlist _ [] = []
filterlist f (x:xs)
    | f x = x:filterlist f xs
    | otherwise = filterlist f xs


maplist :: (a -> b) -> [a] -> [b]
maplist _ [] = []
maplist f (x:xs) = f x:maplist f xs


deletelast :: [a] -> [a]
deletelast [] = error "out of range"
deletelast [x] = []
deletelast (x:xs) = deletelast xs


ziplist :: (Num a, Num b, Num c) => (a -> b -> c) -> [a] -> [b] -> [c]
ziplist _ [] [] = []
ziplist f (x1:xs1) (x2:xs2) = f x1 x2:ziplist f xs1 xs2


slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)


puttogethersortedlists :: Ord a => [a] -> [a] -> [a]
puttogethersortedlists [] [] = []
puttogethersortedlists [] xs = xs
puttogethersortedlists xs [] = xs
puttogethersortedlists (x1:xs1) (x2:xs2)
    | x1 < x2 = x1:puttogethersortedlists xs1 (x2:xs2)
    | otherwise = x2:puttogethersortedlists (x1:xs1) xs2

mergesort :: Ord a => [a] -> [a]
mergesort [x] = [x]
mergesort xs = puttogethersortedlists (mergesort a) (mergesort b)
    where a = slice 0 (ceiling (fromIntegral(len xs)/2)) xs
          b = slice (ceiling (fromIntegral (len xs)/2)) (len xs + 1) xs


-- _onceatthetime :: Eq a => [a] -> a -> [a]
-- _onceatthetime [] _ = []
-- _onceatthetime (y:ys) x
--     | x == y = _onceatthetime ys y
--     | otherwise = y:_onceatthetime (y:ys) y

-- onceatthetime :: Eq a => [a] -> [a]
-- onceatthetime (x:xs) = x:_onceatthetime xs x

onceatthetime :: Eq a => [a] -> [a]
onceatthetime (x:y:ys)
    | x == y = onceatthetime (y:ys)
    | otherwise = x:onceatthetime (y:ys)
onceatthetime xs = xs

-- [8,4,7,9,4,2,5,8,0]
-- [4,7,4,2,5,0][8,8,9]
-- [4,7,4,2,5,0][8,8,9]
-- [2,0][4,4][7,5][8,8][9]
-- [0][2][4,4][5][7][8,8][9]

smaller :: (Ord a, Eq a) => a -> [a] -> [a]
smaller _ [] = []
smaller pivot (x:xs)
    | x < pivot = x:smaller pivot xs
    | otherwise = smaller pivot xs

same :: (Ord a, Eq a) => a -> [a] -> [a]
same _ [] = []
same pivot (x:xs)
    | pivot == x = x:same pivot xs
    | otherwise = same pivot xs

greater :: (Ord a, Eq a) => a -> [a] -> [a]
greater _ [] = []
greater pivot (x:xs)
    | x > pivot = x:greater pivot xs
    | otherwise = greater pivot xs

test :: (Ord a, Eq a) => a -> [a] -> [[a]]
test pivot 

check :: (Ord a, Eq a) => a -> [a] -> [[a]]
check pivot xs = [smaller pivot xs, same pivot xs, greater pivot xs]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = quicksort (getelement comparison 0) ++ getelement comparison 1 ++ quicksort (getelement comparison 2)
    where comparison = check x (x:xs)
































-- [5,3,4,1]
-- [5,3][4,1]
-- [5][3][4][1]
-- [3,5][1,4]
-- [1,3,4,5]

-- puttogethersortedlists((puttogethersortedlists (puttogethersortedlists [5] [2]) (puttogethersortedlists [4] [8])) (puttogethersortedlists (puttogethersortedlists [1] [4]) (puttogethersortedlists [6] [3])))


-- puttogethersortedlists (puttogethersortedlists (puttogethersortedlists [5] [2]) (puttogethersortedlists [4] [8])) (puttogethersortedlists (puttogethersortedlists [1] [4]) (puttogethersortedlists [6] [3]))
-- 2 processes:
--     - taking apart
--         taking the list appart in the middle (if uneven: leftside > rightside)
--     - putting together
--         only with 2 pairs:
--             these two pairs are lists so following is happening:
--                 if ONE list is empty, the unempty list is being added to the back of the result-list
--                 if both are empty, it's finished
--                 if none is empty, the first elements of each are being compared and the smallest is being added nat the back



-- insertionsort
    -- [1, 5, 7, 7, 2, 9, 1]           []
    -- [5, 7, 7, 2, 9, 1]              1
    -- [5, 7, 7, 2, 9, 1]              [1]
    -- [7, 7, 2, 9, 1]                 5
    -- [7, 7, 2, 9, 1]                 [1, 5]
    -- [7, 2, 9, 1]                    7
    -- [7, 2, 9, 1]                    [1, 5, 7]
    -- [2, 9, 1]                       7
    -- [2, 9, 1]                       [1, 5, 7, 7]
    -- [9, 1]                          2
    -- [9, 1]                          [1, 2, 5, 7, 7]
    -- [9]                             9
    -- [9]                             [1, 2, 5, 7, 7, 9]
    -- []                              1
    -- []                              [1, 1, 2, 5, 7, 7, 9]


-- mergesort ::
    -- [6,4,2,6,8,9,4]

    -- [6,4,2,6] [8,9,4]

    -- [6,4] [2,6] [8,9] [4]

    -- [6] [4] [2] [6] [8] [9] [4]

    -- [4,6] [2,6] [8,9] [4]
    --     [2]         [4]
    --    [2,4]      [4,8,9]
    --   [2,4,6]     [4,8,9]
    --  [2,4,6,6]    [4,8,9]

    -- [2,4,6,6] [4,8,9]
    --         [2]
    --        [2,4]
    --       [2,4,4]
    --      [2,4,4,6]
    --     [2,4,4,6,6]
    --    [2,4,4,6,8,9]
