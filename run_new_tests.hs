#!/usr/bin/env runhaskell

-- Simple test runner for NewCabalTestSpec
import Test.Tasty
import Test.Unit.NewCabalTestSpec

main :: IO ()
main = defaultMain tests