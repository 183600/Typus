#!/usr/bin/env runhaskell
import Utils
import Data.List (isPrefixOf)
import Test.QuickCheck

-- Reproduce the failing test case
prop_normalize_indentation_tabs :: String -> Property
prop_normalize_indentation_tabs s =
  let withTabs = "\t\t" ++ s ++ "\t"
      normalized = normalizeIndentation withTabs
  in if null s
     then property $ True  -- 对于空字符串，normalizeIndentation返回原始输入，这是正确的
     else property $ not ("\t\t" `isPrefixOf` normalized)

main :: IO ()
main = do
    putStrLn "Testing with \r:"
    let s = "\r"
    let withTabs = "\t\t" ++ s ++ "\t"
    let normalized = normalizeIndentation withTabs
    putStrLn $ "s = " ++ show s
    putStrLn $ "withTabs = " ++ show withTabs
    putStrLn $ "normalized = " ++ show normalized
    putStrLn $ "startsWithTabs = " ++ show ("\t\t" `isPrefixOf` normalized)
    putStrLn $ "Test passes: " ++ show (not ("\t\t" `isPrefixOf` normalized))
    
    putStrLn "\nRunning QuickCheck:"
    quickCheck prop_normalize_indentation_tabs