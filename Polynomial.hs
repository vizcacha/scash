module Polynomial where

data Polynomial = Polynomial [Int]
	deriving (Eq)

instance Num (Polynomial) where
	Polynomial [] + Polynomial [] = Polynomial []
	Polynomial [] + Polynomial a = Polynomial a
	Polynomial a + Polynomial [] = Polynomial a
	Polynomial (a:as) + Polynomial (b:bs) =
		let Polynomial h = Polynomial as + Polynomial bs in Polynomial ((a+b):h)

	Polynomial [] * Polynomial [] = Polynomial []
	Polynomial [] * Polynomial a  = Polynomial []
	Polynomial a  * Polynomial [] = Polynomial []
	Polynomial (a:as) * Polynomial b =
		(Polynomial [a * eb | eb <- b]) + (Polynomial as * Polynomial (0:b)) 

instance Show Polynomial where
	show (Polynomial []) = "0"
	show (Polynomial (p:ps)) = 
		let
			makePolyStr(_, []) = ""
			makePolyStr(n, 0:ls) = makePolyStr(n+1, ls)
			makePolyStr(n, l:ls) = " + " ++ (show l) ++ "x**" ++ (show n) ++ makePolyStr(n+1, ls)
		in (show p) ++ makePolyStr(1, ps)

derivative :: Polynomial -> Polynomial
derivative (Polynomial []) = Polynomial []
derivative (Polynomial a) =
	let h = diff 0 a in Polynomial h where
		diff :: Int -> [Int] -> [Int]
		diff _ [] = []
		diff n (l:ls) = (n * l):(diff (n+1) ls)    