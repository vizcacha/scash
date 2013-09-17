module Polynomial_Test where

import Polynomial
import Test.HUnit

testAddEmpty = TestCase $ assertEqual "Adding two empty polynomials" (Polynomial []) ((Polynomial []) + (Polynomial []))

tests = TestList [testAddEmpty]

main = runTestTT tests