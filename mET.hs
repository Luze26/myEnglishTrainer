import qualified Data.Text as T
import Data.Char
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
play  i [] = do putStrLn $ "Done in " ++ ((intToDigit i) : " shots !!")
play i list = do
		rand <- randomRIO (0 :: Int, length list - 1)
		putStrLn $ drawWord rand list
		l <- getLine
		let solutions = tail $ list !! rand in do
		putStrLn $ foldl (\deck x -> "\n+" ++ x ++ deck) "" $ solutions
		if correct l $ solutions
			then do
				putStrLn ":)" 
				play (i+1) $ let (ys,zs) = splitAt rand list in ys ++ (tail zs)
			else do
				putStrLn ":(" 
				play (i+1) list
		return ()

createList :: String -> [[String]]
createList input = [ map T.unpack $ head x : (T.splitOn slash $ last x) | x <- map (T.splitOn tab) $ map T.pack $ lines input]
	where tab = T.pack "\t"
	      slash = T.pack "/"


drawWord :: Int -> [[String]] -> String
drawWord r list = list !! r !! 0

correct :: String -> [String] -> Bool
correct _ [] = False
correct rep (x:xs)
	| rep==x = True
	| otherwise = correct rep xs