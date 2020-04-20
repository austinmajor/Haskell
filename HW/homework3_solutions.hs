-- Homework #3
-- Due on: Friday 03/06/2020
--
--
--Grading Rubric:
-- 100%: works on all given examples AND other general cases
-- 75%: works on all given examples but not on similar general cases
-- 50%: works on part of given examples
-- 25%: works on particular one (1) example
-- 0%: makes no sense or cheats


{-|
1. [10 marks] PLEASE provide function type declaration. 

The Greek mathematician Nicomachus devised a 3-way classification of all natural numbers based on their aliquot sum. The aliquot sum of any integer n > 0 is defined as the sum of all factors of n, not including n itself. For example, the aliquot sum of 15 is (1 + 3 + 5) = 9, and the aliquot sum of 28 is (1 + 2 + 4 + 7 + 14) = 28. The number n is called deficient if its aliquot sum is smaller than n; it is called perfect if its aliquot sum is n, and is called abundant if its aliquot sum is larger than n.

Write a function classify n, which takes an integer n as input, and returns its classification. The classification of any integer smaller than 1 can be stated as illegal.

The Haskell interaction may look like: 
> classify 12 
"Abundant" 
> classify 0 
"Illegal"
> classify 28
"Perfect"
> classify 8 
"Deficient"
-}
-- classify n 

classify :: Integer -> String
classify n
    | n < 1       = "Illegal"
    | aliquot < n = "Deficient"
    | aliquot > n = "Abundant"
    | otherwise   = "Perfect"
    where
        aliquot = sum [ i | i <- [1 .. (n-1)], n `mod` i == 0 ]

{-|
2. [10 marks] PLEASE provide function type declaration. 

Write a function bin2dec s, which takes as input a string s containing a binary number, and returns that number (in decimal). As a special case, it returns 0 for the empty string.

HINT: You may use recursion and split the given string by the built-in functions init and last.

The Haskell interaction may look like:
> binInt "1110" 
14
> binInt "0" 
0
> binInt (take 4 (cycle "01")) 
5
> binInt " " 
0
> binInt "1"
1


"-}

-- bin2dec s = undefined

bin2dec :: String -> Integer

bin2dec "" = 0
bin2dec "0" = 0
bin2dec "1" = 1
bin2dec s = 2 * bin2dec (init s) + bin2dec ([last s])




{-|
3. [10 marks] PLEASE provide function type declaration. 

USE RECURSION to design a calculator that computes combinations. An example is avaiable at https://www.calculatorsoup.com/calculators/discretemathematics/combinations.php

The Combinations Calculator will find the number of possible combinations that can be obtained by taking a sample of items from a larger set. Basically, it shows how many different possible subsets can be made from the larger set. For this calculator, the order of the items chosen in the subset does not matter.


The Haskell interaction may look like:
    > nCr 0 0  
        1
    > nCr 3 2  
        3
    > nCr 8 3  
        56
-}

-- nCr n r = undefined

nCr:: Double -> Double -> Double
nCr _ 0 = 1
nCr n 1 = n
nCr n r = ((n * (n-1)) / (r * (r-1))) * (nCr (n-2) (r-2))
