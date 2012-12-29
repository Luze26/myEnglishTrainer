import qualified Data.Text as T
import System.Random( randomRIO )
import System.IO
import System.Environment( getArgs )


main = do
	args <- getArgs
	handle <- openFile (args !! 0) ReadMode  
	contents <- hGetContents handle  
	play 0 $ createList contents
	hClose handle



play :: Int -> [[String]] -> IO ()
play  i [] = do
		putStr "Done in "
		print i
		putStrLn "shots !!"
play i list = do
		rand <- randomRIO (0 :: Int, length list - 1)
		putStrLn $ drawWord rand list
		l <- getLine
		print $ tail $ list !! rand
		if correct l $ list !! rand 
			then do
				putStrLn ":)" 
				play (i+1) $ let (ys,zs) = splitAt rand list in ys ++ (tail zs)
			else do
				putStrLn ":(" 
				play (i+1) list
		return ()



createList :: String -> [[String]]
createList input = [ head x : (map T.unpack (T.splitOn (T.pack "/") (T.pack $ unwords $ tail x))) | x <- map words $ lines input]



drawWord :: Int -> [[String]] -> String
drawWord r list = list !! r !! 0



correct :: String -> [String] -> Bool
correct _ [] = False
correct rep (x:xs) = if (rep==x) then True else correct rep xs