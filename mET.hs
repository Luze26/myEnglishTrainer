import qualified Data.Text as T
import System.Random( randomRIO )
import System.IO
import System.Environment( getArgs )


-- main ////////////////////////////////////////////////////////////////
main = do
	args <- getArgs
	contents <- readFile (args !! 0)
	play 0 $ createList contents


-- play ////////////////////////////////////////////////////////////////
play :: Int -> [[String]] -> IO ()
play  i [] = do putStrLn $ "Done in " ++ (show i) ++ " shots !!"
play i list = do
		rand <- randomRIO (0 :: Int, length list - 1)
		let 	tuple = list !! rand 
			solutions = tail tuple
			in do
				putStrLn $ head tuple
				answer <- getLine
				if correct answer tuple
				then do
					putStrLn $ response True solutions
					play (i+1) $ let (ys,zs) = splitAt rand list in ys ++ (tail zs)
				else do
					putStrLn $ response False solutions
					play (i+1) list
				return ()


-- createList ////////////////////////////////////////////////////////////////
createList :: String -> [[String]]
createList input = [ map T.unpack $ head x : (T.splitOn slash $ last x) | x <- map (T.splitOn tab) $ map T.pack $ lines input]
	where tab = T.pack "\t"
	      slash = T.pack "/"

	      
-- correct ////////////////////////////////////////////////////////////////
correct :: String -> [String] -> Bool
correct _ [] = False
correct rep (x:xs)
	| rep==x = True
	| otherwise = correct rep xs


-- response ////////////////////////////////////////////////////////////////
response :: Bool -> [String] -> String
response b solutions
	| b == True = "=)" ++ solutionsString
	| otherwise = "=\\" ++ solutionsString
	where solutionsString = foldl (\deck sol -> "\n\t-" ++ sol ++ deck) "" solutions