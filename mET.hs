import qualified Data.Text as T
import Definition
import System.Random( randomRIO )
import System.IO
import System.Environment( getArgs )


-- main ////////////////////////////////////////////////////////////////
main = do
	args <- getArgs
	contents <- readFile (args !! 0)
	play 0 $ createList contents

-- play ////////////////////////////////////////////////////////////////
play :: Int -> [Definition] -> IO ()
play  i [] = putStrLn $ "Done in " ++ (show i) ++ " shots !!"
play i list = do
		rand <- randomRIO (0 :: Int, length list - 1)
		let 	(Definition {word = wr, solution = solutions}) = list !! rand
			in do
				putStrLn wr
				answer <- getLine
				if correct answer solutions
				then do
					putStrLn $ response True solutions
					play (i+1) $ let (ys,zs) = splitAt rand list in ys ++ (tail zs)
				else do
					putStrLn $ response False solutions
					play (i+1) list
				return ()

-- createList ////////////////////////////////////////////////////////////////
createList :: String -> [Definition]
createList input = [ readDef x | x <- lines input]

-- response ////////////////////////////////////////////////////////////////
response :: Bool -> [String] -> String
response b solutions
	| b == True = "=)" ++ solutionsString
	| otherwise = "=\\" ++ solutionsString
	where solutionsString = foldl (\deck sol -> "\n\t-" ++ sol ++ deck) "" solutions