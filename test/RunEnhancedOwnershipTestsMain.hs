-- | Enhanced Ownership Tests Runner
-- 
-- Usage:
--   stack test typus:enhanced-ownership-tests
--   or
--   runhaskell test/RunEnhancedOwnershipTests.hs

module Main (main) where

import RunEnhancedOwnershipTests (runEnhancedOwnershipTests)

main :: IO ()
main = runEnhancedOwnershipTests