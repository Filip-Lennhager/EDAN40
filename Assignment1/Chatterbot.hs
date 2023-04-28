{-Assignment N1 Chatterbot-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe (isJust)

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------
{-
stateOfMind generates a random Float value and transforms the BotBrain by leaving the pattern 
phrases unchanged and randomly selecting a response phrase from the list of possible responses 
using the random Float value. The resulting function, wrapped in an IO context, can be applied 
to a Phrase to obtain a transformed Phrase based on the rules and randomly selected responses 
from the BotBrain.
-}
stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
stateOfMind bb = do
  r <- randomIO :: IO Float
  return (rulesApply (map (map2 (id, pick r)) bb))

{-
rulesApply applies the transformations to the input 
phrase according to the provided pattern pairs.
-}
rulesApply :: [PhrasePair] -> Phrase -> Phrase
rulesApply = try . transformationsApply "*" reflect 

{-
reflectString attempts to find a "reflection" of the input string in 
the reflections list using the lookup function. If a match is found, 
the function returns the corresponding reflection; if not, it returns 
the original input string. 
-}
reflectString :: String -> String
reflectString = try (`lookup` reflections)

{-
reflect applies the reflectString function to each word in the input 
phrase using the map function. The purpose of this function is to 
"reflect" pronouns and other words in the input phrase according 
to the reflections list.
-}
reflect :: Phrase -> Phrase
reflect = map reflectString


reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------
{- 
Determines if the input string signals the end of the dialog by checking if it's equal to "quit". 
-}
endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

{-
Converts a phrase into a single string by concatenating the words with spaces. 
-}
present :: Phrase -> String
present = unwords

{-
Prepares an input string to be processed as a phrase by:
  * Removing punctuation characters
  * Converting the string to lowercase
  * Splitting the string into words
  * Applying the reduce function
-}
prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

{- 
Compiles a list of (String, [String]) tuples into a BotBrain by:
  * Converting each string to lowercase and splitting it into words
  * Applying this transformation to both the pattern and the list of responses
-}
rulesCompile :: [(String, [String])] -> BotBrain
rulesCompile = (map.map2) (f, map f)
  where f = words . map toLower


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
reductionsApply = fix . try . transformationsApply "*" id



-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
{-
substitute is a function that takes a wildcard element (wc), a target list (t:ts), and a substitution
list (s) as input. It replaces all occurrences of the wildcard element in the target list with the 
substitution list and returns the resulting list. If the target list is empty, it returns an empty list.
-}
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wc [] s = []
substitute wc (t:ts) s 
  | t == wc   = s ++ substitute wc ts s
  | otherwise = t : substitute wc ts s

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wc [] [] = Just []
match wc [] s = Nothing
match wc p [] = Nothing 
match wc (p:ps) (s:ss) 
  | wc == p   = orElse (singleWildcardMatch (p:ps) (s:ss)) (longerWildcardMatch (p:ps) (s:ss))
  | p == s    = match wc ps ss
  | otherwise = Nothing


-- Helper function to match
{-
If the pattern list without the wildcard wc matches the rest of the target list (xs),
singleWildcardMatch returns the first element of the target list wrapped in a Just constructor; 
otherwise, it returns Nothing.

longerWildcardMatch tries to match the rest of the pattern list (wc:ps) with the rest of the target list (xs),
and if successful, it appends the first element of the target list to the matched sublist
and returns the new sublist wrapped in a Just constructor.
-}
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs)
  | isJust (match wc ps xs)   = Just [x]
  | otherwise                 = Nothing 

longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
{-
transformationApply attempts to match the target list (x) with the first pattern 
list (p1). If successful, it applies the transformation function (f) to the matched 
sublist and substitutes the result for the wildcard element in the second pattern 
list (p2), returning the resulting list wrapped in a Just constructor. If the match 
is unsuccessful, it returns Nothing.
-}
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wc f x (p1,p2) = mmap (substitute wc p2 . f) (match wc p1 x)


-- Applying a list of patterns until one succeeds
{-
transformationsApply takes a wildcard element (wc), a transformation function (f),
a list of pattern tuples (plist), and a target list (x). It tries to apply each
pattern tuple in the list to the target list using transformationApply until one
of them succeeds. If successful, it returns the transformed list wrapped in a Just
constructor. If none of the patterns match, it returns Nothing.
-}
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wc f plist x = foldl1 orElse (map (transformationApply wc f x) plist)