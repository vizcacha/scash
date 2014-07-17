module Main (
    main
) where

import Polynomial
import Test.HUnit

testAddEmpty = TestCase $ assertEqual "Adding two empty polynomials" (Polynomial []) ((Polynomial []) + (Polynomial []))
testAdd1 = TestCase $ assertEqual "Adding two non-empty polynomials" (Polynomial [Monomial 1 0, Monomial 4 1, Monomial (-2) 2, Monomial 1 3]) (Polynomial [Monomial 1 0, Monomial (-3) 2] + Polynomial [Monomial 4 1, Monomial 1 2, Monomial 1 3])
additionTests = TestList [testAddEmpty, testAdd1]

testMultiplyEmpty = TestCase $ assertEqual "Multiplying two empty polynomials" (Polynomial []) (Polynomial [] * Polynomial [])
testMultiplyByEmpty = TestCase $ assertEqual "Multiplying by an empty polynomial yields the empty polynomial" (Polynomial []) (Polynomial [] * Polynomial [Monomial 1 0, Monomial (-4) 1])
testMultiply1 = TestCase $ assertEqual "Multiplying two Polynomials" (Polynomial [Monomial 1 0, Monomial 3 1, Monomial 3 2, Monomial 1 3]) (Polynomial [Monomial 1 0, Monomial 2 1, Monomial 1 2] * Polynomial [Monomial 1 0, Monomial 1 1])  
multiplicationTests = TestList [testMultiply1, testMultiplyEmpty, testMultiplyByEmpty]

testDerivativeZero = TestCase $ assertEqual "The derivative of zero is zero" (Polynomial []) (derivative $ Polynomial [])
testDerivativeConstant = TestCase $ assertEqual "The derivative of a constant is zero" (Polynomial []) (derivative $ Polynomial [Monomial 3 0])
testDerivativeNonConstant = TestCase $ assertEqual "The derivative of x^2 is 2x" (Polynomial [Monomial 2 1]) (derivative $ Polynomial [Monomial 1 2])
derivativeTests = TestList [testDerivativeZero]

tests = TestList [additionTests, multiplicationTests, derivativeTests]

main = runTestTT tests
