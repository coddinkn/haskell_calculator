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

calcPerends :: String -> Int
calcPerends (p:x:op:y:q)
	| op == '+' = ((digitToInt x) + (digitToInt y))
    | op == '-'	= ((digitToInt x) - (digitToInt y))
	| op == '*' = ((digitToInt x) * (digitToInt y))
calcPerends (p:x:op:q)
	| op == '!' = (fac (digitToInt x))
	| x == '-' = ((digitToInt op) * (-1))

replacePerends :: String -> Int -> String
replacePerends str x = if isNothing n
					then error "no bracket enclosed expression present"
					else filter (/= 'X') (replacePerends' str x (fromJust n))
				where n = elemIndex ')' str  

replacePerends' :: String -> Int -> Int -> String
replacePerends' str x n = if (str!!n) /= '('
						then replacePerends' (take n str ++ ['X'] ++ drop (n + 1) str) x (n - 1)
						else (take n str ++ [(intToDigit x)] ++ drop (n + 1) str)

calculate :: String -> String
calculate str = if countPerends str /= 0
				then calculate (replacePerends str (calcPerends (getFirstPerends str)))
				else str
