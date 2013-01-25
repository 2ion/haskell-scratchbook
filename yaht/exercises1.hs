-- exercises from YAHT

import Data.Char
import Data.Maybe

-- #3.3

maptobool :: [Char] -> [Bool]
maptobool list = map isLower list

-- #3.4

nooflower :: [Char] -> Int
nooflower list = length (filter isLower list)

-- #3.5

maxinlist :: [Int] -> Int
maxinlist list = foldl (max) 0 list

-- #3.6

fvalpair :: [(Int,Char)] -> Int
fvalpair list = fst (list !! 1)

-- #3.7

myfib :: Int -> Int
myfib 1 = 1
myfib 2 = 1
myfib n = myfib (n-1) + myfib (n-2)

-- #3.8 

mymult :: Integer -> Integer -> Integer
mymult 0 m = 0
mymult n 0 = 0
mymult 1 m = m
mymult n 1 = n
mymult n m = n + (mymult n (m-1))

-- works with negative numbers
mymult2 :: Int -> Int -> Int
mymult2 m 0 = 0
mymult2 0 n = 0
mymult2 m 1 = m
mymult2 1 n = n
mymult2 n m = 
    let makelist n m l  
            | length l == n = l
            | otherwise = makelist n m (m : l)
    in foldl (+) 0 (makelist n m [])

-- bad solution
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f list = 
    let g h [] m = reverse m
        g h (x:xs) m = g h xs ((h x) : m)
    in  g f list []

-- still bad
mymap2 :: (a -> b) -> [a] -> [b]
mymap2 f [] = []
mymap2 f list =
    let g h l m i
            | length l == i = reverse m
            | otherwise = g h l ((h (l !! i)) : m) (i+1)
    in  g f list [] 0

-- list comprehension
mymap3 :: (a -> b) -> [a] -> [b]
mymap3 f [] = []
mymap3 f l = [ f x | x <- l ]

-- #3.10


main = do
    putStrLn (show (maptobool "Hello"))
    putStrLn (show (nooflower "Hello"))
    putStrLn (show (maxinlist [3,7,23,42,1337]))
    putStrLn (show (maxinlist []))
    putStrLn (show (fvalpair [(1,'a'), (2, 'b')]))
    putStrLn (show (myfib 9))
    putStrLn (show (mymult 7 6))
    putStrLn (show (mymult2 7 (-6)))
    putStrLn (show (mymap toUpper "Hello"))
    putStrLn (show (mymap2 toUpper "Hello"))
    putStrLn (show (mymap3 toUpper "Hello"))
