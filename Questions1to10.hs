-- Problem 1
-- Find the last element of a list.

myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- Problem 2
-- Find the last but one element of a list.

myButLast :: [x] -> x
myButLast [] = error "Empty list"
myButLast [x] = x
myButLast (x : xs) 
    | length xs == 1 = x
    | otherwise = myButLast xs

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt (x : _) 1 = x
elementAt (_ : xs) k = elementAt xs (k -1)

-- Problem 4
-- Find the number of elements of a list.

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

-- Problem 5
-- Reverse a list.

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

-- Solution 1
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

--Solution 2
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' x 
    | head x == last x = isPalindrome' (init (tail x))
    | otherwise = False

-- Problem 7
-- Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

data NestedList a = Elem a | List [NestedList a]

myFlatten (List []) = []
myFlatten (Elem x) = [x] 
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)

-- Problem 8
-- If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

compress [] = []
compress [x] = [x]
compress (x:y:ys)
    | x == y = compress (y:ys)
    | otherwise = x:compress(y:ys)

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

-- Solution 1
pack x = reverse(packhelper x [] [])

packhelper [] z w = z:w
packhelper [x] z w = (x:z):w
packhelper (x:y:ys) z w 
    | x == y = packhelper (y:ys) (x:z) w
    | otherwise = packhelper (y:ys) [] ((x:z):w)

--Solution 2
pack' [] = []
pack' [x] = [[x]]
pack' (x:xs) 
    | x `elem` head (pack' xs) = (x:head (pack' xs)):tail (pack' xs) 
    | otherwise = [x]:pack' xs
