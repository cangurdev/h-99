-- Problem 14
-- Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs 

-- Problem 15
-- Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli [] k = []
repli (x:xs) k = (replicate k x) ++ (repli xs k)

-- Problem 16
-- Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery a k = dropHelper a k 1

dropHelper :: [a] -> Int -> Int -> [a]
dropHelper [] _ _ = []
dropHelper (x:xs) k j 
	| j `mod` k == 0 = [] ++ dropHelper xs k (j+1)
	| otherwise = [x] ++  dropHelper xs k (j+1) 
	
-- Problem 17
-- Split a list into two parts; the length of the first part is given.

split :: [a] -> Int ->([a],[a])
split a k =((take k a), take ((length a) - k) (reverse a))

-- Problem 18
-- Extract a slice from a list.

slice :: [a] -> Int -> Int -> [a]
slice (x:xs) k j
        | k > 2 = slice (xs) (k-1) (j-1)
        | otherwise = take (j-k+1) xs
