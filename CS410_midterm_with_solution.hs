--
--
-- Single choice:
-- https://bbhelp.eku.edu/controlling-printing-blackboard-tests

-- 
-- Given the following definition, what is the value of fun1 2?
-- A.2 B.4 C.8 D.16 E. None of the above
-- D
fun1 :: Integer -> Integer 
fun1 n
    |(n==4) =n
    |otherwise = 2 * fun1 (n + 1)


-- Given the following definition, what is the value of fun2 4 3?
fun2 :: Integer -> Integer -> Integer 
fun2 x y
    |(x==0) =y
    |otherwise = fun2 (x-1) (x+y)
-- A.13 B.12 C.9 D.10 E. None of the above
-- A


-- Given the following definition, what is the value of fun3 [12, 7, 13]?
--
fun3 :: [Integer] -> Integer 
fun3 [] = 0
fun3 (h:t)
    |((h`mod`2)==0) = h + rest 
    |otherwise = h-rest 
    where rest = fun3 t
-- A. 6 B.7 C.8 D.12 E. None of the above
-- A


-- What does the function fun4 compute?
fun4 :: Integer -> Integer -> Integer 
fun4 x y
    |(y==0) =0
    |otherwise = x + (fun4 x (y - 1))
-- A. 𝑥+𝑦 B. 𝑥+𝑥𝑦 C. 𝑥𝑦 D. 𝑥! E. None of the above
-- C


-- Given the following definition, which expression has the value "D"?
fun6 :: String -> String 
fun6 [] = []
fun6 (h:t)
    | (h `elem` ['A'..'Z']) = [h]
    | otherwise = fun6 t

-- A. fun6 "Break Dance"
-- B. fun6 "break dance"
-- C. fun6 "break Dance"
-- D. fun6 (fun6 "Break Dance")
-- E. None of the above
-- C



--  Given the following definitions, what is the value of fun8 10?
fun8 :: Integer -> Integer
fun8 result = case () of _ 
                            | result > 1    -> 1
                            | result > 10   -> 10
                            | otherwise     -> 100

-- A. 1
-- B. 10
-- C. 100
-- D. None of the above
-- A


--
--
-- Given the following definitions, which one filling out the blank will give a function that is equivalent to "take m ys"?
-- A. fun9 (n-1) xs ++ [x]
-- B. x : fun9 (n-1) xs
-- C. x ++ fun9 (n-1) xs
-- D. x + fun9 (n-1) xs
-- E. None of the above
-- B

fun9 m ys = case (m,ys) of
                 (0,_)       ->  []
                 (_,[])      ->  []
                 -- (n,x:xs)    -> _________________ 

-- Given the following definitions, what is the ouput of fun10 10?
data Floatnum = MakeFloat Integer
fun10 x 
    | x < 0     = error "Out of range." 
    | otherwise = MakeFloat x

-- A. 10.0
-- B. A data of a type MakeFloat
-- C. A data of a type Floatnum
-- D. Depends on how MakeFloat is defined
-- E. None of the above
-- C


-- Given the following definitions, what is the ouput of "fun11 (+(-4)) 2"?
fun11 :: (a -> a) -> a -> a
fun11 f x = f (f (f x))
-- A. -12
-- B. 2
-- C. -6
-- D. -10
-- E. None of the above
-- D


-- Given the following definitions, what is the ouput of "fun12 (+1) [1..5]"?
fun12 :: (a -> b) -> [a] -> [b]
fun12 _ [] = []
fun12 f (x:xs) = f x : fun12 f xs

-- A. [2..6]
-- B. [1,3,6,10,15]
-- C. [2,4,7,11,16]
-- D. [6,5,4,3,2]
-- E. None of the above
-- A


-- Given the following definitions, what is the ouput of "fun13 [1..3]"?
fun13 = (\a -> case a of {[] -> []; (x:xs) -> [sum xs]}) 
-- A. [5]
-- B. [1,5,6]
-- C. []
-- D. [6]
-- E. None of the above
-- A



-- Given the following definitions, what is the ouput of "fun14"?
fun14 = let x = 0 : y
            y = 1 : x
        in  y
-- A. [1,0,1,0,...]
-- B. [0,1,0,1,...]
-- C. [1,0]
-- D. [1]
-- E. None of the above
-- A



-- Given the following definitions, what is the ouput of "fun15 1234"?
fun15 n = if (mod n 4 == 0 && mod n 100 /=0) || (mod n 400 == 0)
            then True
            else False
-- A. True
-- B. False
-- C. None of the above
-- B


-- fun 16

-- Given the following definitions, what is the ouput of "fun16 "1a2b3c4d""?
fun16 x = [y | y<-x,y `elem` ['a'..'z']] ++ [z | z<-x,z `elem` ['0'..'9']] 
-- A. "1234"
-- B. "abcd"
-- C. "1234abcd"
-- D. "abcd1234"
-- E. None of the above
-- D


-- fun 17: 


-- Given the following definitions, what is the ouput of "fun17 "abcdefghijklmn""?
fun17 s = null [x|x <- zip ['A'..'Z'] ['a'..'z'], not (fst x `elem` s), not (snd x `elem` s)]
-- A. True
-- B. False
-- C. None of the above
-- B

-- Given the following definitions, what is the ouput of "fun18 8"?
fun18 n
    | n < 1  = "A"
    | me < n = "B"
    | me > n = "C"
    | otherwise   = "D"
    where
        me = sum [ i | i <- [1 .. (n-1)], n `mod` i == 0 ]


-- A."A"
-- B."B"
-- C."C"
-- D."D"
-- E. None of the above
-- B


-- Given the following definitions, what is the ouput of "fun19 1 3"?

fun19 a b = sum (map (^2) [a..b])

-- A. 14
-- B. 12
-- C. 9
-- D. 6
-- E. None of the above
-- A

-- Given the following definitions, what is the ouput of "fun20 "1010""?
fun20 "" = 0
fun20 "0" = 0
fun20 "1" = 1
fun20 s = 2 * fun20 (init s) + fun20 ([last s])

-- A. 1010
-- B. 2
-- C. 6
-- D. 10
-- E. None of the above
-- D
        

-- Short Answers:
-- 1. Consider the following definition of a mystery function: 
mystery :: [Integer] -> [Integer]
mystery x = [ sum (take i x) | i <- [1 .. length x] ]

-- a. Use a number example to explain the function of "mystery". Describe the relationship between the input and the output.
-- b. Please implement an equivalent function to the above definition by using recursion.
--

--
-- 2. Sort program
--
--
-- Please briefly explain with an example the function definitions and the algorithm.

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

--
--
-- Please briefly explain with an example the function definitions and the algorithm.

bubblesort'iter :: (Ord a) => [a] -> [a]
bubblesort'iter (x:y:xs)
    | x > y = y : bubblesort'iter (x:xs)
    | otherwise = x : bubblesort'iter (y:xs)
bubblesort'iter (x) = (x)


bubblesort' :: (Ord a) => [a] -> Int -> [a]
bubblesort' xs i 
    | i == (length xs) = xs
    | otherwise = bubblesort' (bubblesort'iter xs) (i + 1) 
 
bubblesort :: (Ord a) => [a] -> [a]
bubblesort xs = bubblesort' xs 0


-- Please briefly explain with an example the function definitions and the algorithm.
mergesort'merge :: (Ord a) => [a] -> [a] -> [a]
mergesort'merge [] xs = xs
mergesort'merge xs [] = xs
mergesort'merge (x:xs) (y:ys)
    | (x < y) = x:mergesort'merge xs (y:ys)
    | otherwise = y:mergesort'merge (x:xs) ys
 
mergesort'splitinhalf :: [a] -> ([a], [a])
mergesort'splitinhalf xs = (take n xs, drop n xs)
    where n = (length xs) `div` 2 
 
mergesort :: (Ord a) => [a] -> [a]
mergesort xs 
    | (length xs) > 1 = mergesort'merge (mergesort ls) (mergesort rs)
    | otherwise = xs
    where (ls, rs) = mergesort'splitinhalf xs





-- This is the answer.
-- mystery' [] = []
-- mystery' (x:xs) = x : (map (+x) $ mystery' xs )



