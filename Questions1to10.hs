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
myButLast (x : xs) = if length xs == 1 then x else myButLast xs

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

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x
