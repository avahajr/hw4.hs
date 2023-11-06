import System.Exit (die)
import System.Directory.Internal.Prelude (getArgs)
import System.Environment (getProgName)
import Data.Char (isLower)
import Data.Set (Set)
import qualified Data.Set as Set

{-

Problem 2: Word Ladder

Write a program that takes the name of a dictionary file and starting
and ending words and constructs the shortest "word ladder," i.e., a
sequence of dictionary words that differ by exactly one changed
character that transforms the starting word into the ending word.
Unlike some word ladder puzzles, your program should not consider
adding or removing letters.

Your program should read a Unix-style /usr/dict/words file (i.e., a
sorted, newline-separated list of valid words), and only consider
lowercase-only words (i.e., no capitalized proper nouns).  You may
also ignore words with punctuation, e.g., abase is fine, but ignore
aardvark's, ASCII, and Abby.

The user must supply a valid dictionary file and start and end words
that are the same length, all lowercase, and both in the dictionary.
Your program should print an error message and terminate with a
non-zero error code if supplied with erroneous arguments.

Search for ladders of at most 20 words, returning an error message if
one cannot be found with the given dictionary.  For a given pair of
words and dictionary, there is often more than one minimum-length
ladder; you may return any of them.

Here is an example run; your program should work with other
dictionaries, too.

$ wget https://raw.githubusercontent.com/eneko/data-repository/master/data/words.txt
$ stack --resolver lts-21.9 ghc -- --make -Wall -O wordLadder.hs
$ ./wordLadder words.txt fool sage
fool
foot
fort
fore
fare
fage
sage
$ ./wordLadder words.txt computer solution
Unable to find a ladder in 20
$ ./wordLadder
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo bar
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo bar baz
wordLadder: foo: openFile: does not exist (No such file or directory)
$ ./wordLadder words.txt fooa barb
"fooa" not in dictionary
$ ./wordLadder words.txt barb fooa
"fooa" not in dictionary
$ ./wordLadder words.txt bar none
"bar" and "none" must be the same length

As for the wordFreq problem, make your solution correct, readable, and
fast (in order of importance).  Again, we will classify the
performance of your solution into one of three categories:
substantially faster than our reference solution, about the same, and
substantially slower.

Our 68-line reference solution uses a BFS-style search implemented
with functions from System.IO, System.Exit, Data.Char,
System.Environment, Data.Set, Control.Monad, and Data.List.

For reference, the fool->sage example above took about 220 ms on my
desktop machine.  Our solution uses one fairly straightforward trick
to improve its performance, but is otherwise untuned.

-}

type Dict = Set String

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename, start, end] -> do
      if length start /= length end
      then die $ show start ++ " and " ++ show end ++ " must be the same length"
      else let sl = length start in 
        do
          text <- readFile filename
          let dict = Set.fromList(filter (\w -> length w == sl && all isLower w) (words text)) in
            if start `notElem` dict
            then die $ show start ++ " is not in dictionary"
            else if end `notElem` dict
            then die $ show end ++ " is not in dictionary"
            else case findWordLadder dict start end of
              Nothing -> putStrLn "Unable to find a ladder in 20"
              Just optimalPath -> mapM_ putStrLn optimalPath
    _ -> do
      pn <- getProgName
      die $ "Usage: " ++ pn ++ " <dictionary-filename> <from-word> <to-word>"

adj :: String -> String -> Bool
"" `adj` "" = False
"" `adj` _ = False
_ `adj` "" = False
(x:xs) `adj` (y:ys) 
  | x == y = xs `adj` ys
  | otherwise = xs == ys

getAdjSet :: String -> Dict -> Set String
getAdjSet s = Set.filter (`adj` s) 

findWordLadder :: Dict -> String -> String -> Maybe [String]
findWordLadder dict start end = bfs [(start, [start])] Set.empty 0
  where
    bfs :: [(String, [String])] -> Set String -> Int -> Maybe [String]
    bfs [] _ _ = Nothing
    bfs ((word, path):rest) visited step
      | word == end = Just (reverse path)
      | step >= 20 = Nothing
      | otherwise = let nextWords = getAdjSet word dict 
                        unvisitedNextWords = Set.difference nextWords visited
                        newPaths = [(w, w:path) | w <- Set.toList unvisitedNextWords]
                        step' = if null rest then step + 1 else step
                    in bfs (rest ++ newPaths) (Set.insert word visited) step'