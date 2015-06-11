-- this code is currently learn you a haskell's rpn calculator

import Data.List
import Data.Maybe

fac :: Float -> Float
fac 0.0 = 1.0
fac x = x * fac (x - 1)

computeRPN :: String -> Float
computeRPN = head . foldl foldingFunction [] . words
	where   foldingFunction (x:y:ys) "*" = (x * y):ys
		foldingFunction (x:y:ys) "+" = (x + y):ys
		foldingFunction (x:y:ys) "-" = (y - x):ys
		foldingFunction (x:y:ys) "/" = (y / x):ys
		foldingFunction (x:y:ys) "^" = (y ** x):ys
		foldingFunction (x:xs) "ln" = log x:xs
		foldingFunction (x:xs) "!" = fac x:xs 
		foldingFunction xs "sum" = [sum xs]
		foldingFunction xs numberString = read numberString:xs

-- my attempt at an infix calculator starts here

isPerend :: Char -> Bool
isPerend c = c == '(' || c == ')'

countPerends = length . filter isPerend

getFirstPerends :: String -> String
getFirstPerends str = if isNothing n
						then "no bracket enclose expression present"
						else getFirstPerends' str (fromJust n) ""
					where n = elemIndex ')' str 

getFirstPerends' :: String -> Int -> String -> String
getFirstPerends' str n list = if (str!!n) /= '('
									then getFirstPerends' str (n - 1) ((str!!n):list)
									else ('(':list)

 
