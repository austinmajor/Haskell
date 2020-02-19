-- Homework #2
-- Due on: Wednesday 02/19/2020
--
--
--Grading Rubric:
-- 100%: works on all given examples AND other general cases
-- 75%: works on all given examples but not on similar general cases
-- 50%: works on part of given examples
-- 25%: works on particular one (1) example
-- 0%: makes no sense or cheats


{-|
1. [10 marks] Design the function which should print a single digit number as English text, or "unknown" if it's out of the range 0-9. The function takes in an "Int" and outputs a "String".

NOTE: Please provide function declaration [2 marks].
Hint: You may use a list of strings for your outputs. 
 
The Haskell interaction may look like:
    > readDigit 2
        "two"
    > readDigit 9
        "nine"
    > readDigit 25
        "unknown"
-}

readDigit :: (Integral x) => x -> String
readDigit 1 = "one"
readDigit 2 = "two"
readDigit 3 = "three"
readDigit 4 = "four"
readDigit 5 = "five"
readDigit 6 = "six"
readDigit 7 = "seven"
readDigit 8 = "eight"
readDigit 9 = "nine"
readDigit x = "unknown"

{-|
[10 marks] PLEASE IMPLEMENT THE FUNTION USING "GUARDS".

The function leap n, which takes an integer n as input, and returns True if the year n is a leap year, and False otherwise.

NOTE: Please provide function declaration [2 marks].

Leap years are those that are evenly divisible by 4, except any year that is also evenly divisible by 100 unless that year is also evenly divisible by 400. So, for example, 1996, 2012, and 2020 are all leap years, but 2100, 2200, and 2300 are not leap years, because although they are all evenly divisible by 4, they are also evenly divisible by 100. However, 1600, 2000, and 2400 are leap years, because although they are divisible by 100, they are also divisible by 400.

The Haskell interaction may look like:
    > leap 1996 
        True
    > leap 2000
        True
    > leap 2100 
        False
-}

leap::Int->Bool
leap y
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False

{-| [10 marks] 
The function pangram s, which takes a string s as input, and returns True if s is a pangram, and False otherwise.

Pangrams are strings that contain at least one occurrence of each letter of the English alphabet. They are not case-sensitive, so a letter may appear in either the upper case, or lower, or both.

The Haskell interaction may look like:

    > pangram "The quick brown fox jumps over a lazy dog."
    True
    > pangram "Pack my box with five dozen liquor jugs!!"
    True
    > pangram "Watch JEOPARDY!, Alex Trebek’s fun TV quiz game!"
    True
    > pangram "I wanna be a Pangram, can I? :)"
    False
    > pangram "Amazingly few discotheques provide jukeboxes." 
    True
    > pangram "Back in June, we delivered oxygen equipment of the same size."
    True
    > pangram "My girl wove six dozen plaid jackets before she quit :("
    True

Hints: 
1. You may use the below codes to convert a Char to lower or upper cases.

import Data.Char
toLower x
toUpper x

2. If you don't want to use the builtin functin, you can use `zip` to generate a list of pairs for comparison/test.

zip ['A'..'Z'] ['a'..'z']

-}

import Data.Char (toLower)
import Data.List ((\\))
 
pangram :: String -> Bool
pangram = null . (['a' .. 'z'] \\) . map toLower

pangram s = null [x | x <- zip ['A'..'Z'] ['a'..'z'], not (fst x `elem` s), not (snd x `elem` s)]