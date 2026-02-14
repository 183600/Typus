module Main where

import Test.Tasty
import Test.Unit.NewComprehensiveQuickCheckSpec (comprehensiveTests)

main :: IO ()
main = defaultMain comprehensiveTests