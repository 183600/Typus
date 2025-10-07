-- | Main entry point for running type system tests
-- 
-- Usage:
--   stack test typus:type-system-tests
--   or
--   runhaskell test/RunTypeSystemTests.hs

module Main (main) where

import TypeSystemTestSuite (runTypeSystemTests)

main :: IO ()
main = runTypeSystemTests