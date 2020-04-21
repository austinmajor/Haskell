{-
1) Pattern matching
    - fib
2) Recursion
    - maximum'
3) Higher order functions
    - passing functions as variabls
4) Sorting algorithms
    - quicksort 
    - mergesort
    - bubblesort
5) Monad
    - Basic I/O
6) Curried functions
    - partially applied functions
7) zip, map, filter, list operations (take, sum, length, ...)
8) type and typeclasses: Integer, Ord, Show, ...
-}

-- 1) Pattern matching
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- 2) Recursion
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- another example:
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- 3) Passing of functions as if they are variables
times4 :: Int -> Int
times4 x = x * 4

-- 4) Please briefly explain with an example the function definitions and the algorithm.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

-- Example
fun3 :: [Integer] -> Integer 
fun3 [] = 0
fun3 (h:t)   
    |((h`mod`2)==0) = h + rest    
    |otherwise = h-rest    
    where
        rest = fun3 t