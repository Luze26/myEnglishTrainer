import Definition
import qualified Data.Text as T
import System.IO
import System.Environment( getArgs )

-- main ////////////////////////////////////////////////////////////////
main = do
	args <- getArgs
	addWord $ args !! 0


-- addWord ////////////////////////////////////////////////////////////////
addWord :: String -> IO ()
addWord path = do
		putStrLn "Word/Term ?"
		word <- getLine
		sol <- addTranslations solution
		putStrLn "Binder ?"
		lexicon <- getLine
		append ((show Definition {word = word, solutions = sol}) ++ "\n") path $ map T.unpack $ T.splitOn (T.pack " | ") $ T.pack lexicon


-- append ////////////////////////////////////////////////////////////////
append :: String -> String -> [String] -> IO ()
append _ _ [] = return ()
append def path (x:xs) = do 	appendFile (path ++ x) def
				append def path xs


-- addTranslations ////////////////////////////////////////////////////////////////
addTranslations :: Solutions -> IO Solutions
addTranslations sol = do
	putStrLn "* n : add noun\n* v : add verb\n* adj : add adjective\n* p : add pronoun\n* adv : add adverb\n* c : add conjunction\n* t : add term\n* f : add the word"
	choix <- getLine
	if choix == "f" then return sol
	else do
		putStrLn "Translation ?"
		translation <- getLine
		case choix of	"n" 	-> addTranslations $ addTranslation Noun sol translation
				"v" 	-> addTranslations $ addTranslation Verb sol translation
				"adj" 	-> addTranslations $ addTranslation Adjective sol translation
				"p" 	-> addTranslations $ addTranslation Pronoun sol translation
				"adv" 	-> addTranslations $ addTranslation Adverb sol translation
				"c" 	-> addTranslations $ addTranslation Conjunction sol translation
				"t" 	-> addTranslations $ addTranslation Term sol translation
				_ 	-> addTranslations sol