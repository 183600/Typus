{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewTextProcessingBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , splitByComma
  , splitByCommaCollapsed
  , removeLineComments
  , removeComments
  , normalizeIndentation
  , forceSingleTabIndentation
  , fixIndentation
  , breakOn
  )

import Data.Char (isSpace, isControl, isAscii)
import qualified Data.Text as T
import qualified Data.List as Data.List
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (sort)

-- | Text processing boundary tests
tests :: TestTree
tests =
  testGroup "New Text Processing Boundary Tests"
    [ testGroup "Unicode L.and special character handling"
        [ testCase "trim handles Unicode whitespace correctly" $ do
            trim "\x2003hello\x2002world\x00A0" @?= "hello\x2002world"
            
        , testCase "splitBy handles Unicode delimiters" $ do
            splitBy '，' "你好，世界，测试" @?= ["你好", "世界", "测试"]
            
        , testCase "removeLineComments handles Unicode in strings" $ do
            let input = "text := \"你好//世界\" // 注释"
                expected = "text := \"你好//世界\" "
            removeLineComments input @?= expected
        ]
        
    , testGroup "Control character edge cases"
        [ fastProperty "trim preserves control characters in content" $
            \content -> 
                let hasControl = L.any isControl content
                    trimmed = trim content
                    contentPreserved = not (null trimmed) ==> 
                        L.any (not . isSpace) trimmed
                in classify hasControl "has control characters" $
                   property contentPreserved
                    
        , testCase "splitBy handles null characters" $ do
            splitBy '\0' "a\0b\0c" @?= ["a", "b", "c"]
            
        , testCase "removeComments handles embedded control characters" $ do
            let input = "code /*\n\tcomment\n*/ more"
                expected = "code  more"
            removeComments input @?= expected
        ]
        
    , testGroup "Large input performance boundaries"
        [ testCase "splitBy on very long string doesn't crash" $ do
            let longInput = replicate 1000000 'a' ++ "," ++ replicate 1000000 'b'
                result = splitBy ',' longInput
            L.length result @?= 2
            L.length (L.head result) @?= 1000000
            L.length (last result) @?= 1000000
            
        , testCase "removeComments on large nested structure" $ do
            let largeComment = "/* " ++ replicate 50000 'x' ++ " */"
                input = "start " ++ largeComment ++ " end"
                expected = "start  end"
            removeComments input @?= expected
        ]
        
    , testGroup "Edge case string patterns"
        [ testCase "removeComments handles malformed block comments" $ do
            removeComments "code /* unclosed" @?= "code "
            removeComments "code */ malformed" @?= "code */ malformed"
            
        , testCase "removeLineComments handles multiple slashes" $ do
            let input = "x ///// comment"
                expected = "x /////"
            removeLineComments input @?= expected
            
        , fastProperty "splitByCollapsed handles L.all delimiter patterns" $
            \delim content ->
                let result = splitByCollapsed delim content
                    hasNoEmpty = L.all (not . null) result
                    onlyDelimiters = L.all (== delim) content
                    expectedEmpty = onlyDelimiters ==> null result
                in property $ hasNoEmpty .&&. expectedEmpty
        ]
        
    , testGroup "Memory efficiency edge cases"
        [ testCase "trim on extremely nested whitespace" $ do
            let nestedWhitespace = L.concat (replicate 10000 " \t\n\r")
                result = trim nestedWhitespace
            result @?= ""
            
        , testCase "breakOn efficiency with repeating patterns" $ do
            let pattern = "pattern"
                repeated = L.concat (replicate 10000 (pattern ++ "x"))
                (before, after) = breakOn pattern repeated
            before @?= ""
            L.length after @?= L.length repeated
        ]
        
    , testGroup "Unicode normalization edge cases"
        [ testCase "removeComments handles Unicode in block comments" $ do
            let input = "code /* 中文注释 */ more"
                expected = "code  more"
            removeComments input @?= expected
            
        , testCase "splitBy handles combining characters" $ do
            let input = "e\u0301:e\u0301" -- e with acute accent
                result = splitBy ':' input
            result @?= ["e\u0301", "e\u0301"]
        ]
    ]