module Definition
(	Definition(..),
	correct,
	readDef
) where

import qualified Data.Text as T

data Definition = Definition 	{ word :: String,
				  solution :: [String]
				}
				
-- correct ////////////////////////////////////////////////////////////////
correct :: String -> [String] -> Bool
correct _ [] = False
correct rep (x:xs)
	| rep==x = True
	| otherwise = correct rep xs
	
-- read ////////////////////////////////////////////////////////////////
readDef :: String -> Definition
readDef input = let tuple = T.splitOn tab $ T.pack input in Definition (T.unpack $ head tuple) (map T.unpack $ T.splitOn slash $ last tuple)
	where tab = T.pack "\t"
	      slash = T.pack "/"