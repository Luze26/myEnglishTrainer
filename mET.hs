import qualified Data.Text as T
import qualified Data.Char as C
import Control.Monad
import System.Random
import System.IO

main = do
	handle <- openFile "/media/DATA/Cours/Anglais/D2" ReadMode  
	contents <- hGetContents handle  
	jouer 0 $ createList contents
	hClose handle

jouer :: Int -> [[String]] -> IO ()
jouer  i [] = do
		putStr "Fini en "
		print i
		putStrLn "coups!!"
jouer i list = do
		rand <- randomRIO (0 :: Int, length list - 1)
		putStrLn $ tirerMot rand list
		l <- getLine
		print $ tail $ list !! rand
		if juste l $ list !! rand 
			then do
				putStrLn ":)" 
				jouer (i+1) $ let (ys,zs) = splitAt rand list in ys ++ (tail zs)
			else do
				putStrLn ":(" 
				jouer (i+1) list
		return ()

		
createList :: String -> [[String]]
createList input = [ head x : (map T.unpack (T.splitOn (T.pack "/") (T.pack $ unwords $ tail x))) | x <- map words $ lines input]

tirerMot :: Int -> [[String]] -> String
tirerMot r list = list !! r !! 0

juste :: String -> [String] -> Bool
juste _ [] = False
juste rep (x:xs) = if (rep==x) then True else juste rep xs