import Control.Monad
import Data.Char
import Data.List
import Data.String
import Text.Read (readMaybe)
import System.IO



-- read the file
-- create lists left and right
-- sort the lists
-- get another list for their difference
-- sum the new list
-- print
--

parsedLocations :: String -> [[String]]
parsedLocations inp = map words $ lines inp

left :: [[String]] -> [Int]
left xs = [read (head x) :: Int | x <- xs]

right :: [[String]] -> [Int]
right xs = sort $ [read (head $ tail x) :: Int | x <- xs]

distance :: [Int] -> [Int] -> [Int]
distance left right = sort $ [abs $ subtract (fst x) (snd x) | x <- zip (sort left) (sort right)]

main = do
    -- putStr "List -> "
    -- let locations = "1 2\n 5 4"
    -- putStrLn $ show $ sum (distance (left $ parsedLocations locations) (right $ parsedLocations locations)) 

    handle <- openFile "input.txt" ReadMode
    locations <- hGetContents handle
    putStrLn $ show $ sum (distance (left $ parsedLocations locations) (right $ parsedLocations locations)) 
    hClose handle
