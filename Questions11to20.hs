-- Problem 14
-- Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs 

-- Problem 15
-- Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli xs k = concatMap (replicate k) xs

-- Problem 16
-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery a k = dropHelper a k 1

dropHelper :: [a] -> Int -> Int -> [a]
dropHelper [] _ _ = []
dropHelper (x:xs) k j 
    | j `mod` k == 0 = dropHelper xs k (j+1)
    | otherwise = x : dropHelper xs k (j+1) 

-- Problem 17
-- Split a list into two parts; the length of the first part is given.

split :: [a] -> Int ->([a],[a])
split a k =(take k a, take (length a - k) (reverse a))

-- Problem 18
-- Extract a slice from a list.

slice :: [a] -> Int -> Int -> [a]
slice (x:xs) k j
        | k > 2 = slice xs (k-1) (j-1)
        | otherwise = take (j-k+1) xs

-- Problem 19
-- (**) Rotate a list N places to the left.

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate x 0 = x
rotate x y 
   | y > 0 = rotate (tail x ++ [head x]) (y-1)
   | otherwise = rotate (last x : init x) (y+1)

-- Problem 20
-- (*) Remove the K'th element from a list.

removeAt 1 (x:xs) = (x,xs)
removeAt n (x:xs) = (l,x:r)
    where (l,r) = removeAt (n-1) xs 