
import System.IO

parseReports :: String -> [[Int]]
parseReports reports = [map (read::String->Int) $ words x | x <- lines reports]

isSafe :: [Int] -> [Int] -> Bool
isSafe [] _ = True
isSafe x:[] _ = True
isSafe (x:y:xs) ys =
    where change = x - y
          absChange = abs change
          isValidLevel
            | absChange >= 1 && absChange <=3 = True
			| otherwise = False


main = do
    handle <- openFile "test_input.txt" ReadMode
    reports <- hGetContents handle
    putStrLn $ show $ parseReports reports
    hClose handle
