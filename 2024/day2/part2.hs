
import System.IO

parseReports :: String -> [[Int]]
parseReports reports = [map (read::String->Int) $ words x | x <- lines reports]

isChangingWithTolerance :: (Ord a) => (a -> a -> Bool) -> Int ->  [a] -> Bool
isChangingWithTolerance f t (x:[]) = True
isChangingWithTolerance f t (x:xs) = toleranceCount >= 0 && isChangingWithTolerance f toleranceCount nextIterList
    where toleranceCount = if isChanging then t else t-1
          isChanging = x `f` (head xs)
          nextIterList = if isChanging then xs else x:(tail xs)


isIncreasingOrDecreasingWithTolerance :: Int -> [Int] -> Bool
isIncreasingOrDecreasingWithTolerance t xs = isChangingWithTolerance (<) t xs || isChangingWithTolerance (>) t xs


isValidChangeWithTolerance :: Int -> [Int] -> Bool
isValidChangeWithTolerance t [] = True
isValidChangeWithTolerance t (x:[]) = True
isValidChangeWithTolerance t (x:xs) = toleranceCount >= 0 && isValidChangeWithTolerance toleranceCount nextIterList
    where toleranceCount= if isValidChange then t else t-1 
          change = abs $ x - (head xs)
          isValidChange = change >= 1 && change <= 3
          nextIterList = if isValidChange then xs else x:(tail xs)


isSafe :: [Int] -> Bool
isSafe [] = True
isSafe xs = isIncreasingOrDecreasingWithTolerance 1 xs && isValidChangeWithTolerance 1 xs 


safeCount :: [[Int]] -> Int 
safeCount [] = 0
safeCount (x:xs)
   | isSafe x = 1 + safeCount xs
   | otherwise = 0 + safeCount xs

loopAndLog :: [[Int]] -> IO ()
loopAndLog [] = return ()
loopAndLog (x:xs) = do
    putStrLn $ show $ x
    putStrLn $ show $ isSafe x
    loopAndLog xs

	-- putStrLn $ show $ safeCount $ parseReports reports
main = do
    handle <- openFile "input.txt" ReadMode
    reports <- hGetContents handle
    loopAndLog $ parseReports reports
    hClose handle
