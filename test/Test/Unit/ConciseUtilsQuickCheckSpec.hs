module Test.Unit.ConciseUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, (===))
import Data.Char (isSpace)
import qualified Data.List as L
import Data.List (isPrefixOf, isSuffixOf)
import Utils

-- | 简洁的QuickCheck测试，针对Utils模块的核心功能
tests :: TestTree
tests =
  testGroup "Concise Utils QuickCheck Tests"
    [ testGroup "String processing properties"
        [ testProperty "trim idempotent" $
            \s -> trim (trim s) === trim s
            
        , testProperty "trim removes only leading/trailing whitespace" $
            \s -> not (null s) ==> 
            let trimmed = trim s
                hasLeading = not (null s) && isSpace (L.head s)
                hasTrailing = not (null s) && isSpace (last s)
            in if hasLeading || hasTrailing 
               then L.length trimmed < L.length s
               else trimmed === s
               
        , testProperty "splitBy preserves total characters" $
            \c s -> splitBy c s `sumLengths` L.length s
            where
              sumLengths xs n = L.sum (map L.length xs) + L.length xs - 1 === n
              
        , testProperty "splitByCollapsed never produces empty strings" $
            \c s -> L.all (not . null) (splitByCollapsed c s)
            
        , testProperty "splitByCollapsed result L.length <= splitBy result L.length" $
            \c s -> L.length (splitByCollapsed c s) <= L.length (splitBy c s)
            
        , testProperty "splitByComma equals splitBy with comma" $
            \s -> splitByComma s === splitBy ',' s
        ]
    ]

-- Helper function for property testing
infix 1 `sumLengths`