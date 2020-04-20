{-|
The goal of this assignment is to prepare your computer for running Haskell programs, and for you to learn the basics of editing/compiling/executing by writing simple Haskell functions. So it is essential that you complete this assignment soon in order to become ready for later assignments. Please carry out the following installation step on your personal computer: 
  * Download the Haskell Platform: Visit https://www.haskell.org/platform/ and install the Haskell Platform for the operating system on your computer.
 
Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
You can load (and reload) this file in the interpreter with the command: ":l homework1.hs"
-}
 


{-|
1. [10 marks] The function leap n, which takes an integer n as input, and returns True if the year n is a leap year, and False otherwise.

Leap years are those that are evenly divisible by 4, except any year that is also evenly divisible by 100 unless that year is also evenly divisible by 400. So, for example, 1996, 2012, and 2020 are all leap years, but 2100, 2200, and 2300 are not leap years, because although they are all evenly divisible by 4, they are also evenly divisible by 100. However, 1600, 2000, and 2400 are leap years, because although they are divisible by 100, they are also divisible by 400.

The Haskell interaction may look like:
    > leap 1996 
        True
    > leap 2000
        True
    > leap 2100 
        False
-}

-- leap n = undefined
leap n = if (mod n 4 == 0 && mod n 100 /=0) || (mod n 400 == 0)
            then True
            else False


{-|
2. [10 marks] The function "parse id", which takes your WSU ID as input, and returns a string with characters and digits regrouped. 

The Haskell interaction may look like:
    > parse "A123B456" 
    "AB123456"
    > parse "P234Q446" 
    "PQ234446"

-}

-- parse id = undefined
parse id = head id : id !! 4 : take 3 (tail id) ++ take 3 ( drop 5 id )

parse' x = [y | y<-x,y `elem` ['A'..'Z']] ++ [z | z<-x,z `elem` ['0'..'9']] 
