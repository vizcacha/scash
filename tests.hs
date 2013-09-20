module Polynomial_Test where

import Polynomial
import Test.HUnit

testAddEmpty = TestCase $ assertEqual "Adding two empty polynomials" (Polynomial []) ((Polynomial []) + (Polynomial []))
testAdd1 = TestCase $ assertEqual "Adding two non-empty polynomials" (Polynomial [1, 4, -2, 1]) (Polynomial [1, 0, -3] + Polynomial [0, 4, 1, 1])
additionTests = TestList [testAddEmpty, testAdd1]

testMultiplyEmpty = TestCase $ assertEqual "Multiplying two empty polynomials" (Polynomial []) (Polynomial [] * Polynomial [])
testMultiplyByEmpty = TestCase $ assertEqual "Multiplying by an empty polynomial yields the empty polynomial" (Polynomial []) (Polynomial [] * Polynomial [1, -4])
testMultiply1 = TestCase $ assertEqual "Multiplying two Polynomials" (Polynomial [1, 3, 3, 1]) (Polynomial [1, 2, 1] * Polynomial [1, 1])  
multiplicationTests = TestList [testMultiply1, testMultiplyEmpty, testMultiplyByEmpty]

testDerivativeZero = TestCase $ assertEqual "The derivative of zero is zero" (Polynomial []) (derivative $ Polynomial [])
testDerivativeConstant = TestCase $ assertEqual "The derivative of a constant is zero" (Polynomial []) (derivative $ Polynomial [3])
testDerivativeNonConstant = TestCase $ assertEqual "The derivative of x^2 is 2x" (Polynomial [0, 2]) (derivative $ Polynomial [0,0,1])
derivativeTests = TestList [testDerivativeZero]

tests = TestList [additionTests, multiplicationTests, derivativeTests]

main = runTestTT tests