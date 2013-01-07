module Definition
(	Definition(..),
	Solutions,
	Type(..),
	correct,
	readDef,
	solution,
	addTranslation,
	showSolutions
) where

import qualified Data.Text as T

data Type = Noun | Verb | Adjective | Pronoun | Adverb | Conjunction | Term deriving (Eq)

data Solutions = Solutions {	
				nouns :: [String],
				verbs :: [String],
				adjectives :: [String],
				pronouns :: [String],
				adverbs :: [String],
				conjunctions :: [String],
				terms :: [String]
			} deriving (Show, Read)


data Definition = Definition {	
				word :: String,
				solutions :: Solutions
			} deriving (Show, Read)


-- correct ////////////////////////////////////////////////////////////////
correct :: String -> Solutions -> Bool
correct answer solutions = correct' answer $ concatSolutions solutions


-- correct' ////////////////////////////////////////////////////////////////
correct' :: String -> [String] -> Bool
correct' _ [] = False
correct' answer (x:xs)
	| answer==x = True
	| otherwise = correct' answer xs


-- concatSolutions ////////////////////////////////////////////////////////////////
concatSolutions :: Solutions -> [String]
concatSolutions Solutions {nouns = n, verbs = v, adjectives = adj, pronouns = p, adverbs = adv, conjunctions = c, terms = t} = n ++ v ++ adj ++ p ++ adv ++ c ++t


-- read ////////////////////////////////////////////////////////////////
readDef :: String -> Definition
readDef input = read input


-- solution ////////////////////////////////////////////////////////////////
solution :: Solutions
solution = Solutions {nouns = [], verbs = [], adjectives = [], pronouns = [], adverbs = [], conjunctions = [], terms = []}


-- addTranslation ////////////////////////////////////////////////////////////////
addTranslation :: Type -> Solutions -> String -> Solutions
addTranslation ty (Solutions {nouns = n, verbs = v, adjectives = adj, pronouns = p, adverbs = adv, conjunctions = c, terms = t}) trans
	| ty==Noun = Solutions {nouns = trans : n, verbs = v, adjectives = adj, pronouns = p, adverbs = adv, conjunctions = c, terms = t}
	| ty==Verb = Solutions {nouns = n, verbs = trans : v, adjectives = adj, pronouns = p, adverbs = adv, conjunctions = c, terms = t}
	| ty==Adjective = Solutions {nouns = n, verbs = v, adjectives = trans : adj, pronouns = p, adverbs = adv, conjunctions = c, terms = t}
	| ty==Pronoun = Solutions {nouns = n, verbs = v, adjectives = adj, pronouns = trans : p, adverbs = adv, conjunctions = c, terms = t}
	| ty==Adverb = Solutions {nouns = n, verbs = v, adjectives = adj, pronouns = p, adverbs = trans : adv, conjunctions = c, terms = t}
	| ty==Conjunction = Solutions {nouns = n, verbs = v, adjectives = adj, pronouns = p, adverbs = adv, conjunctions = trans : c, terms = t}
	| ty==Term = Solutions {nouns = n, verbs = v, adjectives = adj, pronouns = p, adverbs = adv, conjunctions = c, terms = trans : t}
	| otherwise = Solutions {nouns = n, verbs = v, adjectives = adj, pronouns = p, adverbs = adv, conjunctions = c, terms = t}


-- showSolutions ////////////////////////////////////////////////////////////////
showSolutions :: Solutions -> String
showSolutions Solutions {nouns = n, verbs = v, adjectives = adj, pronouns = p, adverbs = adv, conjunctions = c, terms = t} = (showType "Nouns" n) ++ (showType "Verbs" v) ++ (showType "Adjectives" adj) ++ (showType "Pronouns" p) ++ (showType "Adverbs" adv) ++ (showType "Conjunctions" c) ++ (showType "Terms" t)


-- showType ////////////////////////////////////////////////////////////////
showType :: String -> [String] -> String
showType _ [] = ""
showType ty sols = foldl (\deck sol -> deck ++ "\n\t-" ++ sol) ("\n   * " ++ ty) sols