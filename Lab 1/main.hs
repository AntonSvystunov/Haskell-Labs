import Data.List
import System.IO

-- Returns number of matches of element in list
countnum :: (Eq a) => a -> [a] -> Int
countnum _ [] = 0
countnum n (x: xs)  
    | n == x = (1 + countnum n xs)
    | otherwise = (countnum n xs)

-- Deletes elements from list that matches 3 times
del3 :: (Eq a) => [a] -> [a]
del3 [] = []
del3 l = [x| x <- l, not ((countnum x l) == 3)]

-- Main function
main :: IO()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
        list = map (read::String->Int) singlewords
    print "input list:"
    print list
    print "result:"
    print (del3 list)
    hClose handle   
