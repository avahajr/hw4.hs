import System.IO (readFile)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Data.Map (fromListWith)
import Data.Char (isSpace, isAlpha, toLower)
{-

Problem 1: Word Frequency Counter

Write a program that prints the 40 most common words and the number of
times they appear in the given text file.  Compile this into a
standalone executable that takes a single filename as a command-line
argument.  Print a usage message if the user does not supply exactly
one argument.

To find words, discard all non-alphabetic characters aside from
whitespace and treat what's left as lowercase.  E.g., treat "Stephen's
humor-challenged" as two words: "stephens" and "humorchallenged"

You may use any of the System or Data modules.  Ask permission from
the instructor on Ed before you use any other modules.  You may not
use the Data.Text.WordCount module.

In my 32-line reference solution, I used Data.Map.fromListWith,
Data.List.sortBy, readFile, System.Exit.die,
System.Environment.getArgs, and System.Environment.getProgName.

In addition to making your solution correct and readable, try to make
your solution go fast, but leave it single-threaded.  We will classify
solutions into three performance categories and assign 10% of the
score based on these categories:

1) Roughly the same as my reference solution (about 2s on the
   Shakespeare example);
2) Noticably faster than the reference solution
3) Noticably slower than the reference solution

E.g., on the Complete Works of Shakespeare

$ stack --resolver lts-21.9 ghc -- --make -Wall -O wordFreq
$ ./wordFreq
Usage: wordFreq <filename>
$ wget http://www.gutenberg.org/files/100/100-0.txt
$ ./wordFreq 100-0.txt

This particular file (updated October 11, 2023) is UTF-8 encoded, but
your solution only needs to work with ASCII (8-bit) characters.  To
reincode it in ASCII,

$ iconv -f UTF-8 -t ASCII//TRANSLIT -o 100-0.ascii 100-0.txt 
$ ./wordFreq 100-0.ascii
30550 the
28487 and
22134 i
21035 to
18859 of
16374 a
14434 you
13186 my
12450 in
11805 that
9750 is
9072 not
8553 with
8252 me
8202 it
8192 for
7600 his
7373 be
7154 this
7066 your
6938 he
6741 but
6269 have
6193 as
5856 thou
5554 him
5442 so
5297 will
4744 what
4632 her
4353 thy
4269 all
4104 by
4079 no
4017 do
3851 shall
3815 if
3725 are
3563 we
3416 thee

My solution disagreed about the number of occurences of "a" in the
UTF-8 file (16359) and the ASCII file (16374), but agreed about all
the other counts.

For reference, my straightforward, untuned solution took about 2s to
run on the Shakespeare file.

-}

main :: IO ()
main = do 
   args <- getArgs
   filename <- case args of 
      [file] -> return file
      _ -> do 
            pn <- getProgName
            die $ "Usage: "++pn++" <filename>"
   text <- readFile filename
   mapM_ (putStrLn . cleanLine) $ lines text

cleanLine :: String -> String
cleanLine line = filter (\x -> isAlpha x || isSpace x) (map toLower line)