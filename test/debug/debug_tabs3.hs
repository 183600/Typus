#!/usr/bin/env runhaskell

import qualified Utils
import Data.List (isPrefixOf)

main :: IO ()
main = do
  -- Test prop_normalize_indentation_tabs failure case: " "
  putStrLn "Testing prop_normalize_indentation_tabs with s = \" \":"
  let s = " "
  let withTabs = "\t\t" ++ s ++ "\t"
  let normalized = Utils.normalizeIndentation withTabs
  putStrLn $ "s: " ++ show s
  putStrLn $ "withTabs: " ++ show withTabs
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "Starts with \\t\\t: " ++ show ("\t\t" `isPrefixOf` normalized)
  putStrLn $ "Test passes: " ++ show (not ("\t\t" `isPrefixOf` normalized))
  putStrLn ""