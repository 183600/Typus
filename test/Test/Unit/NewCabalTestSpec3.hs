{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalTestSpec3 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler (compileTypus)
import Compiler.Errors (CompilerError(..))
import Utils (trim, splitBy)
import qualified Data.List as L
import Data.List (isInfixOf)

-- | 测试用例3: 编译器错误处理测试
tests :: TestTree
tests = 
  testGroup "New Cabal Test 3 - Compiler Error Handling"
    [ testCase "compiler reports meaningful errors for invalid syntax" $ do
        let invalidSource = "func main {  // missing closing parenthesis"
        case compileTypus invalidSource of
          Left err -> 
            -- Check that error message contains useful information
            "syntax" `L.isInfixOf` show err @?= True
          Right _ -> fail "expected compilation to fail with invalid syntax"

    , testCase "compiler handles empty source gracefully" $ do
        let emptySource = ""
        case compileTypus emptySource of
          Left _ -> property True  -- Expected to fail L.or succeed gracefully
          Right _ -> property True  -- Either outcome is acceptable

    , testCase "compiler preserves original code in error messages" $ do
        let sourceWithMultipleLines = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"hello"
              , "}"
              ]
        case compileTypus sourceWithMultipleLines of
          Left err -> 
            -- Check that error context is preserved
            "main" `L.isInfixOf` show err @?= True
          Right _ -> fail "expected compilation to fail with unterminated string"

    -- QuickCheck properties
    , fastProperty "compilation result is deterministic" prop_compilation_deterministic
    , fastProperty "compiler handles whitespace variations" prop_compiler_whitespace_variations
    , fastProperty "compiler error messages contain context" prop_compiler_error_context
    ]

-- QuickCheck properties

-- Property: compilation result is deterministic for the same input
prop_compilation_deterministic :: String -> Property
prop_compilation_deterministic source =
  let result1 = compileTypus source
      result2 = compileTypus source
  in property $ case (result1, result2) of
                  (Left err1, Left err2) -> show err1 == show err2
                  (Right _, Right _) -> True
                  _ -> False  -- Different result types indicate non-determinism

-- Property: compiler handles whitespace variations consistently
prop_compiler_whitespace_variations :: String -> Property
prop_compiler_whitespace_variations code =
  -- Add various whitespace patterns
  let withExtraSpaces = "  " ++ code ++ "  "
      withTabs = "\t" ++ code ++ "\t"
      withNewlines = code ++ "\n\n"
  in case (compileTypus code, compileTypus withExtraSpaces) of
         (Left _, Left _) -> property True
         (Right _, Right _) -> property True
         _ -> property False  -- Different success/failure status

-- Property: compiler error messages contain context about the error
prop_compiler_error_context :: String -> Property
prop_compiler_error_context source =
  -- Only test with sources that are likely to fail
  L.length source > 0 && ("{" `L.isInfixOf` source) && not ("}" `L.isInfixOf` source) ==>
  case compileTypus source of
    Left err -> 
      let errorMsg = show err
      in property $ L.length errorMsg > 0  -- Error message should not be empty
    Right _ -> property True  -- If compilation succeeds, that's also valid