
import System.IO

parseReports :: String -> [[Int]]
parseReports reports = [map (read::String->Int) $ words x | x <- lines reports]

isChanging :: (Ord a) => (a -> a -> Bool) ->  [a] -> Bool
isChanging f (x:[]) = True
isChanging f (x:xs) = x `f` (head xs) && isChanging f xs

isIncreasingOrDecreasing :: [Int] -> Bool
isIncreasingOrDecreasing xs = isChanging (<) xs || isChanging (>) xs

isValidChange :: [Int] -> Bool
isValidChange [] = True
isValidChange (x:[]) = True
isValidChange (x:xs) = change >= 1 && change <= 3 && isValidChange xs
    where change = abs $ x - (head xs)

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe xs = isIncreasingOrDecreasing xs && isValidChange xs 

safeCount :: [[Int]] -> Int 
safeCount [] = 0
safeCount (x:xs)
   | isSafe x = 1 + safeCount xs
   | otherwise = 0 + safeCount xs


main = do
    handle <- openFile "input.txt" ReadMode
    reports <- hGetContents handle
    putStrLn $ show $ safeCount $ parseReports  reports
    hClose handle
