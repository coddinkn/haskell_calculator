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

breakPerends :: String -> [String]
breakPerends str = breakPerends' str ((length str) - 2) [] [] 

breakPerends' :: String -> Int -> String -> [String] -> [String]
breakPerends' str n sacc acc = if n > 0
	then if isDigit (str!!n) && isDigit (str!!(n - 1))
		then breakPerends' str (n - 1) ((str!!n):sacc) acc
		else breakPerends' str (n - 1) [] (((str!!n):sacc):acc) 
	else acc

stringToInt :: String -> Int
stringToInt str = read str :: Int

computeList :: [String] -> String
computeList (x:op:y:[])
	| op == "+" = show ((stringToInt x) + (stringToInt y))

replacePerends :: String -> String -> String
replacePerends str x = if isNothing i
	then error "improperly perenthesized expression"
	else filter (/= 'X') (replacePerends' str x (fromJust i))
	where i = elemIndex ')' str

replacePerends' :: String -> String -> Int -> String
replacePerends' str x n = if (str!!n) /= '('
	then replacePerends' (take n str ++ "X" ++ drop (n + 1) str) x (n - 1)
	else (take n str ++ x ++ drop (n + 1) str)

calculate :: String -> String
calculate exp = if numPerends exp /= 0
	then calculate (replacePerends (removeSpaces exp) (computeList (breakPerends (firstPerends (removeSpaces exp)))))
	else exp	
