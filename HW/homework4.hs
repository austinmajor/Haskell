-- Homework #4
-- Due on: Monday 04/13/2020
-- Austin Major
-- N675Q967

--Grading Rubric:
-- 100%: works on all given examples AND other general cases
-- 75%: works on all given examples but not on similar general cases
-- 50%: works on part of given examples
-- 25%: works on particular one (1) example
-- 0%: makes no sense or cheats

{- Define a function that computes the sum of all integers between two numbers (the two numbers included).
    The Haskell interaction may look like:
    > sumInts 0 1  --  0 + 1 =1
    1
    > sumInts 3 6  --  3 + 4 + 5 + 6 = 18
    18
    > sumInts 6 3
    0
-}

sumInts :: Int -> Int -> Int
sumInts x y
    | x > y = 0
    | otherwise = x + sumInts (x + 1) y

{- Define a function that computes the sum of the squares of all integers between two numbers (the two numbers included).
    The Haskell interaction may look like:
    > sumSquares 0 1  --  0*0 + 1*1 =1
    1
    > sumSquares 3 6  --  3*3 + 4*4 + 5*5 + 6*6 = 86
    86
    > sumSquares 6 3
    0
-}

sumSquares :: Int -> Int -> Int
sumSquares a b
    | a > b = 0
    | otherwise = (a * a) + sumSquares (a + 1) b

{- Define a higher order sum function, higherOrderSum, which accepts an (Int -> Int) function to apply to all integers between two values (the two numbers included), such that sumSquares' and sumInts' defined below have the same functions as sumSquares and sumInts respectively.
-}

higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum fun a b =
  sum $ map fun [a..b]

sumSquares' :: Int -> Int -> Int
sumSquares' = higherOrderSum (^2)

sumInts' :: Int -> Int -> Int
sumInts' = higherOrderSum (*1)

{-
 - Define the I/O function that prompts users to input two integers, and prints the sum of the squares all integers between two numbers (the two numbers included).
    The Haskell interaction may look like:

    > runhaskell homework4.hs
    Please give me one integer:
    > 3   -- user input
    Please give me the other integer:
    > 6 -- user input
    The sum of the squares all integers between 3 and 6 is 86.
-}

main :: IO()
main = do
    print "Please give me one integer: "
    x <- getLine
    let xi = read x :: Int
    print "Please give the other integer: "
    y <- getLine
    let yi = read y :: Int
    putStrLn ("The sum of the squares all integers between " ++ (show xi) ++ " and " ++ (show yi) ++ " is " ++ (show (sumSquares xi yi)) ++ ".")