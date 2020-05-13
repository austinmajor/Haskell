fun11 :: (a -> a) -> a -> a
fun11 f x = f (f (f x))

fun19 a b = sum (map (^2) [a..b])

fun18 n
    | n<1 = "A"
    | me <n = "B"
    | me > n = "C"
    | otherwise = "D"
    where
        me = sum [i | i <- [1..(n-1)], n `mod` i == 0]

fun6 :: String -> String 
fun6 [] = []
fun6 (h:t)
    | (h `elem` ['A'..'Z']) = [h]
    | otherwise = fun6 t

fun13 = (\a -> case a of {[] -> []; (x:xs) -> [sum xs]})

{-
fun20 "" = 0
fun20 "0" = 0
fun20 "1" = 1
fun20 s = 2 * bin2dec (init s) + bin2dec ([last s])
-}

{-
data Floatnum = MakeFloat Integer
fun10 x
    | x < 0 = error "Out of range."
    | otherwise = MakeFloat x
-}

fun16 x = [y | y<-x,y `elem` ['a'..'z']] ++ [z | z<-x,z `elem` ['0'..'9']]

fun12 :: (a -> b) -> [a] -> [b]
fun12 _ [] = []
fun12 f (x:xs) = f x : fun12 f xs

fun15 n = if (mod n 4 == 0 && mod n 100 /=0) || (mod n 400 == 0)
    then True
    else False

fun3 :: [Integer] -> Integer
fun3 [] = 0
fun3 (h:t)
    |((h `mod` 2)==0) = h + rest
    |otherwise = h-rest
    where rest = fun3 t

fun2 :: Integer -> Integer -> Integer
fun2 x y
    |(x==0) = y
    | otherwise = fun2 (x-1) (x+y)

fun4 :: Integer -> Integer -> Integer
fun4 x y
    |(y==0) = 0
    |otherwise = x + (fun4 x (y - 1))

fun1 :: Integer -> Integer
fun1 n
    |(n==4) =n
    |otherwise = 2 * fun1 (n + 1)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]

add :: [Integer] -> [Integer]
add x = [sum (take i x) | i <- [1..length x]]

mystery' :: [Integer] -> [Integer]
mystery' x = [sum (take i x) | i <- [1 .. length x]]

sumThem :: Num a => [a] -> a
sumThem (x:xs) = x + sumThem xs
sumThem []     = 0

mystery :: [Integer] -> [Integer] 
mystery xx = aux xx 0
    where aux [] a = []
          aux (x:xs) a = (a+x) : aux xs (a+x)

sumList :: [Integer] -> [Integer]
sumList n
    |n == [] = 0
    |head(n) < 10 = head(n) + sumList(tail(n))
    |head(n) >= 10 = (sum(digits(head(n)))) + sumList(tail(n))
    |otherwise = 0