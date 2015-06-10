-- this code is currently learn you a haskell's rpn calculator

import Data.List

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
