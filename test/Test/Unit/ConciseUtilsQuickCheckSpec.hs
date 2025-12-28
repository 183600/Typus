module Test.Unit.ConciseUtilsQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty, Property, (===))
import Data.Char (isSpace)
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
                hasLeading = not (null s) && isSpace (head s)
                hasTrailing = not (null s) && isSpace (last s)
            in if hasLeading || hasTrailing 
               then length trimmed < length s
               else trimmed === s
               
        , testProperty "splitBy preserves total characters" $
            \c s -> splitBy c s `sumLengths` length s
            where
              sumLengths xs n = sum (map length xs) + length xs - 1 === n
              
        , testProperty "splitByCollapsed never produces empty strings" $
            \c s -> all (not . null) (splitByCollapsed c s)
            
        , testProperty "splitByCollapsed result length <= splitBy result length" $
            \c s -> length (splitByCollapsed c s) <= length (splitBy c s)
            
        , testProperty "splitByComma equals splitBy with comma" $
            \s -> splitByComma s === splitBy ',' s
        ]
    ]

-- Helper function for property testing
infix 1 `sumLengths`