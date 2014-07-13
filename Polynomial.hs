module Polynomial where

data Monomial = Monomial Int Int
    deriving (Eq)

instance Show Monomial where
    show (Monomial a 0) = show a
    show (Monomial a b) = show a ++ "x^" ++ show b

data Polynomial = Polynomial [Monomial]
    deriving (Eq)

instance Num (Polynomial) where
    Polynomial a + Polynomial b = Polynomial (a ++ b)

instance Show Polynomial where
    show (Polynomial []) = "0"
    show (Polynomial [a]) = show a
    show (Polynomial (p:ps)) = show p ++ "+" ++ show (Polynomial ps)

mderivative :: Monomial -> Monomial
mderivative (Monomial a 0) = Monomial 0 0
mderivative (Monomial a b) = Monomial (a*b) (b-1)

derivative :: Polynomial -> Polynomial
derivative (Polynomial []) = Polynomial []
derivative (Polynomial (l:ls)) = (Polynomial [mderivative l]) + (derivative (Polynomial ls))

simplify :: Polynomial -> Polynomial
simplify (Polynomial []) = Polynomial []
simplify (Polynomial ((Monomial a n):ls)) = 
    let (m, p) = getsimplifiedcoeff (Polynomial ((Monomial a n):ls)) n in
        Polynomial [m] + simplify p  

getsimplifiedcoeff :: Polynomial -> Int -> (Monomial, Polynomial)
getsimplifiedcoeff p n = getsimplifiedcoeffrec p n (Monomial 0 n) (Polynomial [])

getsimplifiedcoeffrec :: Polynomial -> Int -> Monomial -> Polynomial -> (Monomial, Polynomial)
getsimplifiedcoeffrec (Polynomial []) _ m p = (m, p)
getsimplifiedcoeffrec (Polynomial ((Monomial a k):ls)) n (Monomial b l) p = 
    if k == n then getsimplifiedcoeffrec (Polynomial ls) n (Monomial (b+a) l) p
    else getsimplifiedcoeffrec (Polynomial ls) n (Monomial b l) (p + (Polynomial [Monomial a k]))
