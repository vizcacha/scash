module Polynomial where

data Polynomial = Polynomial [Int]
	deriving (Eq, Show)

instance Num (Polynomial) where
	Polynomial [] + Polynomial [] = Polynomial []
	Polynomial [] + Polynomial a = Polynomial a
	Polynomial a + Polynomial [] = Polynomial a
	Polynomial (a:as) + Polynomial (b:bs) =
		let Polynomial h = Polynomial as + Polynomial bs in Polynomial ((a+b):h)