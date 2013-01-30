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
-- see file exercise3.10.hs

-- #4.1
-- 1. [Char]
-- 2. Error: lists must be monotyped
-- 3. (Int, Char) (Double, Char) or Num t => (t, Char), because the type
-- of 5 has not been explicitly stated.
-- 4. Int
-- 5. Error, colliding types (or whatever?)

-- #4.2
-- 1. snd :: (a, b) -> b
-- 2. head :: [a] -> a
-- 3. null :: Bool
-- 4. (head . tail) :: [a] -> a
-- 5. (head . head) :: [[a]] -> a
--
-- #4.3
-- 1. (\x -> [x]) :: a -> [a]
-- 2. (\x y z -> (x, y:z:[])) :: a -> b -> b -> (a, [b])
-- 3. (\x -> x + 5) :: Num a => a -> a
-- 4. (\x -> "Hello World") :: String or (\x -> "Hello World") :: [Char]
-- 5. (\x -> x 'a') :: (Char -> b) -> b
-- 6. (\x -> x x) :: ? (isn't this kind of looping/infinite?)
-- 7. (\x -> x + x) :: Num a => a -> a

-- #4.4

data Triple a b c = Triple a b c

tripleFst :: (Triple a b c) -> a
tripleSnd :: (Triple a b c) -> b
tripleThr :: (Triple a b c) -> c

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

-- #4.5

data Quadruple a b  = Quadruple a a b b

firstTwo :: (Quadruple a b) -> [a]
firstTwo (Quadruple a b c d) = [a, b]

lastTwo :: (Quadruple a b) -> [b]
lastTwo (Quadruple a b c d) = [c, d]

-- #4.6

data Tuple a b c d = One a
    | Two a b
    | Three a b c
    | Four a b c d

tuple1 (One     a) = Just a
tuple1 (Two     a b) = Just a
tuple1 (Three   a b c) = Just a
tuple1 (Four    a b c d) = Just a

tuple2 (One     a) = Nothing
tuple2 (Two     a b) = Just b
tuple2 (Three   a b c) = Just b
tuple2 (Four    a b c d) = Just b

-- and so on

-- #4.7

-- data Either a b = Left a | Right b

ttuple (One     a)          = Left  (Left   a)
ttuple (Two     a b)        = Left  (Right  (a, b))
ttuple (Three   a b c)      = Right (Left   (a, b, c))
ttuple (Four    a b c d)    = Right (Right  (a, b, c, d))

-- #4.8 head, tail, foldl and foldr implementations using the list type

data List a = Nil
    | Cons a (List a)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = Nothing
listHead (Cons x xs) = Just x

listTail Nil = Nothing
listTail (Cons x xs) = Just xs

-- simulation foldl
-- listFoldl (+) 0 [1,2,3,4] -> listFoldl (+) ((+) 1 0) [2,3,4]
--                           -> listFoldl (+) (1) [2,3,4]
--                           -> listFoldl (+) ((+) 2 1) [3,4]
--                           -> listFoldl (+) (3) [4]
--                           -> listFoldl (+) ((+) 4 3) []
--                           -> listFoldl (+) (7) []
--                           -> 7
listFoldl op ival Nil = ival
listFoldl op ival (Cons x xs) = listFoldl op (op x ival) xs

-- FIXME: I can't figure out listFoldr!

data BinaryTree t = Leaf t
    | Branch (BinaryTree t) t (BinaryTree t)

treeSize (Leaf x) = 1
treeSize (Branch left x right) = 1 + treeSize left + treeSize right

-- #4.9 collect all elements of a BinaryTree in a Haskell list

collectTree (Leaf x) = [x]
collectTree (Branch left x right)= [x] ++ (collectTree left) ++ (collectTree right)

-- #4.10a fold function for a BinaryTree

foldTree op ival (Leaf x) = op x ival
foldTree op ival (Branch left x right) = foldTree op (op x (foldTree op ival left)) right

-- #4.11b rewrite collectTree in terms of foldTree

collectTree2 tree = foldTree (:) [] tree

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
