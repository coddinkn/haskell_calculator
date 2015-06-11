-- this code is currently learn you a haskell's rpn calculator

import Data.List
import Data.Maybe
import Data.Char

fac :: Int -> Int
fac 0 = 1
fac x = x * fac (x - 1)

computeRPN :: String -> Float
computeRPN = head . foldl foldingFunction [] . words
	where   foldingFunction (x:y:ys) "*" = (x * y):ys
		foldingFunction (x:y:ys) "+" = (x + y):ys
		foldingFunction (x:y:ys) "-" = (y - x):ys
		foldingFunction (x:y:ys) "/" = (y / x):ys
		foldingFunction (x:y:ys) "^" = (y ** x):ys
		foldingFunction (x:xs) "ln" = log x:xs
		foldingFunction xs "sum" = [sum xs]
		foldingFunction xs numberString = read numberString:xs

-- my attempt at an infix calculator starts here

isPerend :: Char -> Bool
isPerend c = c == '(' || c == ')'

countPerends = length . filter isPerend

getFirstPerends :: String -> String
getFirstPerends str = if isNothing n
						then error "no bracket enclosed expression present"
						else getFirstPerends' str (fromJust n) ""
					where n = elemIndex ')' str 

getFirstPerends' :: String -> Int -> String -> String
getFirstPerends' str n list = if (str!!n) /= '('
								then getFirstPerends' str (n - 1) ((str!!n):list)
								else ('(':list)

getNumbers :: String -> [String]
getNumbers str = [getFirstNumber str, getSecondNumber str]

getFirstNumber :: String -> String
getFirstNumber str = getFirstNumber' str [] 1

getFirstNumber' :: String -> String -> Int -> String
getFirstNumber' str new n = if isDigit (str!!n)
					then getFirstNumber' str (new ++ [str!!n]) (n + 1)
					else new  

getSecondNumber :: String -> String
getSecondNumber str = getSecondNumber' str [] n
				where n = ((fromJust (elemIndex ')' str)) - 1) 

getSecondNumber' :: String -> String -> Int -> String
getSecondNumber' str new n = if isDigit (str!!n)
					then getSecondNumber' str ((str!!n):new) (n - 1)
					else new

calcPerends :: String -> String
calcPerends str = 'a':str

replacePerends :: String -> String -> String
replacePerends str x = if isNothing n
					then error "no bracket enclosed expression present"
					else filter (/= 'X') (replacePerends' str x (fromJust n))
				where n = elemIndex ')' str  

replacePerends' :: String -> String -> Int -> String
replacePerends' str x n = if (str!!n) /= '('
						then replacePerends' (take n str ++ ['X'] ++ drop (n + 1) str) x (n - 1)
						else (take n str ++ x ++ drop (n + 1) str)

calculate :: String -> String
calculate str = if countPerends str /= 0
				then calculate (replacePerends str (calcPerends (getFirstPerends str)))
				else str
