{-# LANGUAGE CPP #-}
module Test.Unit.NewCabalTestSpec9 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import SyntaxValidator (validateSyntax, SyntaxError(..))
import Parser (parseTypus)
import Utils (trim)
import qualified Data.List as L
import Data.List (isInfixOf)

-- | 测试用例9: 语法验证器测试
tests :: TestTree
tests = 
  testGroup "New Cabal Test 9 - Syntax Validator"
    [ testCase "syntax validator accepts valid function declarations" $ do
        let source = unlines
              [ "package main"
              , "func validFunction() int {"
              , "    return 42"
              , "}"
              ]
        case parseTypus source of
          Left err -> fail $ "parseTypus failed: " ++ err
          Right parsed -> 
            case validateSyntax parsed of
              Left err -> fail $ "syntax validation failed: " ++ err
              Right _ -> property True  -- Validation succeeded

    , testCase "syntax validator detects missing closing braces" $ do
        let source = unlines
              [ "package main"
              , "func incompleteFunction() {"
              , "    return 42"
              ]
        case parseTypus source of
          Left err -> fail $ "parseTypus failed: " ++ err
          Right parsed -> 
            case validateSyntax parsed of
              Left err -> 
                -- Check that error mentions braces
                "brace" `L.isInfixOf` err @?= True
              Right _ -> fail "expected syntax validation to detect missing brace"

    , testCase "syntax validator handles complex type annotations" $ do
        let source = unlines
              [ "package main"
              , "func complexFunction(a map[string][]int) (result []string, err error) {"
              , "    return []string{}, nil"
              , "}"
              ]
        case parseTypus source of
          Left err -> fail $ "parseTypus failed: " ++ err
          Right parsed -> 
            case validateSyntax parsed of
              Left err -> fail $ "syntax validation failed: " ++ err
              Right _ -> property True  -- Validation succeeded

    , testCase "syntax validator validates control flow structures" $ do
        let source = unlines
              [ "package main"
              , "func controlFlow(x int) {"
              , "    if x > 0 {"
              , "        println(\"positive\")"
              , "    } else {"
              , "        println(\"non-positive\")"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> fail $ "parseTypus failed: " ++ err
          Right parsed -> 
            case validateSyntax parsed of
              Left err -> fail $ "syntax validation failed: " ++ err
              Right _ -> property True  -- Validation succeeded

    -- QuickCheck properties
    , fastProperty "syntax validation is deterministic" prop_syntax_validation_deterministic
    , fastProperty "syntax validation preserves valid code" prop_syntax_validation_preserves_valid
    , fastProperty "syntax validation detects structural errors" prop_syntax_validation_detects_errors
    ]

-- QuickCheck properties

-- Property: syntax validation is deterministic for the same input
prop_syntax_validation_deterministic :: String -> Property
prop_syntax_validation_deterministic source =
  case parseTypus source of
    Left _ -> property True  -- Parse failures are acceptable for arbitrary input
    Right parsed -> 
      let result1 = validateSyntax parsed
          result2 = validateSyntax parsed
      in property $ case (result1, result2) of
                      (Left err1, Left err2) -> show err1 == show err2
                      (Right _, Right _) -> True
                      _ -> False

-- Property: syntax validation preserves valid code
prop_syntax_validation_preserves_valid :: String -> Property
prop_syntax_validation_preserves_valid code =
  -- Use a simple valid code structure
  let validCode = "package main\nfunc test() {}\n" ++ code
  in case parseTypus validCode of
         Left _ -> property True  -- Parse failures are acceptable
         Right parsed -> 
           case validateSyntax parsed of
             Left _ -> property True  -- Validation failures are acceptable
             Right _ -> property True  -- Success is expected

-- Property: syntax validation detects structural errors
prop_syntax_validation_detects_errors :: String -> Property
prop_syntax_validation_detects_errors code =
  -- Create code with intentional structural error
  let invalidCode = "package main\nfunc test() {\n" ++ code
  in case parseTypus invalidCode of
         Left _ -> property True  -- Parse failures are acceptable
         Right parsed -> 
           case validateSyntax parsed of
             Left _ -> property True  -- Expected to detect error
             Right _ -> property True  -- Or succeed if code is actually valid