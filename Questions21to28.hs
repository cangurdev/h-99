-- Problem 21
-- Insert an element at a given position into a list.

insertAt :: a -> [a] -> Int -> [a]
insertAt x y 1 = x:y
insertAt x (y:ys) z =  y:insertAt x ys (z-1)

-- Problem 22
-- Create a list containing all integers within a given range.
range x y = [x..y]
