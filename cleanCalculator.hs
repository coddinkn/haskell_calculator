-- cleaned up, more haskell-y version of the infix calculator

import Data.List
import Data.Char
import Data.Maybe

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

isPerend :: Char -> Bool
isPerend c = c == '(' || c == ')'

numPerends :: String -> Int
numPerends = length . filter isPerend

firstPerends :: String -> String
firstPerends str = if isNothing i
	then error "improperly perenthesized expression"
	else firstPerends' str (fromJust i) ""
	where i = elemIndex ')' str

firstPerends' :: String -> Int -> String -> String
firstPerends' str i acc = if (str!!i) /= '('
	then firstPerends' str (i - 1) ((str!!i):acc)
	else '(':acc
