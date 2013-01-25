import Data.Maybe
import Data.Char

main = do
    numbers <- getNumbers
    putStrLn "Your numbers added up:"
    putStrLn (show (foldl (+) 0 numbers))
    putStrLn "Your numbers multiplied with each other:"
    putStrLn (show (foldl (*) 1 numbers))
    putStrLn "The factorials of your numbers:"
    putStrLn (show (factorials numbers))

getNumbers = do
    putStrLn "Enter a number or zero to stop:"
    num <- getLine
    if read num == 0
        then return []
        else do
            rest <- getNumbers
            return ((read num :: Int) : rest)

factorials [] = []
factorials list = [ fac x | x <- list ]

fac 1 = 1
fac n = n * fac (n-1)
