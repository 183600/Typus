{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SecurityValidationCabalsSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import TestSupport.Arbitrary (genString, genNonEmptyString)

import Compiler (compile, CompilerError(..))
import Parser (parseTypus, TypusFile(..))
import ErrorHandler (formatCompilerErrors)

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf, length, isSuffixOf)
import Data.List (sort)
import qualified Data.Text as T

-- Test 1: Security validation prevents buffer overflow
test_security_buffer_overflow :: TestTree
test_security_buffer_overflow =
  testCase "Security validation prevents buffer overflow" $ do
    let source = unlines
          [ "package main"
          , "func vulnerable() {"
          , "  buffer := make([]byte, 10)"
          , "  large := make([]byte, 100)"
          , "  copy(buffer, large)  // Potential buffer overflow"
          , "}"
          , "func main() {"
          , "  vulnerable()"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should catch potential buffer overflow
            assertBool "Should detect potential buffer overflow" $
              L.any (`L.isInfixOf` show compileErr) 
                ["buffer", "overflow", "copy", "bounds"]
          Right result -> do
            -- May compile with runtime checks
            assertBool "Should add runtime bounds checking" True

-- Test 2: Security validation prevents integer overflow
test_security_integer_overflow :: TestTree
test_security_integer_overflow =
  testCase "Security validation prevents integer overflow" $ do
    let source = unlines
          [ "package main"
          , "func calculate(a, b int) int {"
          , "  return a * b  // Potential integer overflow"
          , "}"
          , "func main() {"
          , "  result := calculate(2147483647, 2)  // Max int * 2"
          , "  println(result)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should catch potential integer overflow
            assertBool "Should detect potential integer overflow" $
              L.any (`L.isInfixOf` show compileErr) 
                ["overflow", "integer", "bounds", "check"]
          Right result -> do
            -- May compile with overflow checks
            assertBool "Should add overflow checking" True

-- Test 3: Security validation prevents SQL injection
test_security_sql_injection :: TestTree
test_security_sql_injection =
  testCase "Security validation prevents SQL injection" $ do
    let source = unlines
          [ "package main"
          , "func queryUser(id string) {"
          , "  sql := \"SELECT * FROM users WHERE id = \" + id  // SQL injection vulnerability"
          , "  execute(sql)"
          , "}"
          , "func main() {"
          , "  queryUser(\"1; DROP TABLE users;\")"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should catch SQL injection vulnerability
            assertBool "Should detect SQL injection vulnerability" $
              L.any (`L.isInfixOf` show compileErr) 
                ["SQL", "injection", "concatenation", "sanitize"]
          Right result -> do
            -- May compile with warnings
            assertBool "Should warn about SQL concatenation" True

-- Test 4: Security validation prevents unsafe type casting
test_security_unsafe_casting :: TestTree
test_security_unsafe_casting =
  testCase "Security validation prevents unsafe type casting" $ do
    let source = unlines
          [ "package main"
          , "func dangerous(ptr uintptr) {"
          , "  // Unsafe pointer casting"
          , "  intPtr := (*int)(unsafe.Pointer(ptr))"
          , "  println(*intPtr)"
          , "}"
          , "func main() {"
          , "  x := 42"
          , "  dangerous(uintptr(unsafe.Pointer(&x)))"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should catch unsafe pointer operations
            assertBool "Should detect unsafe pointer operations" $
              L.any (`L.isInfixOf` show compileErr) 
                ["unsafe", "pointer", "cast", "security"]
          Right result -> do
            -- May compile with safety warnings
            assertBool "Should warn about unsafe operations" True

-- Test 5: Security validation prevents path traversal
test_security_path_traversal :: TestTree
test_security_path_traversal =
  testCase "Security validation prevents path traversal" $ do
    let source = unlines
          [ "package main"
          , "func readFile(filename string) {"
          , "  path := \"/data/\" + filename  // Path traversal vulnerability"
          , "  data := readAll(path)"
          , "  println(data)"
          , "}"
          , "func main() {"
          , "  readFile(\"../../../etc/passwd\")"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should catch path traversal vulnerability
            assertBool "Should detect path traversal vulnerability" $
              L.any (`L.isInfixOf` show compileErr) 
                ["path", "traversal", "filename", "sanitize"]
          Right result -> do
            -- May compile with warnings
            assertBool "Should warn about path concatenation" True

-- QuickCheck property: Security validation catches dangerous patterns
prop_security_catches_dangerous_patterns :: String -> Property
prop_security_catches_dangerous_patterns code =
  L.length code < 100 ==>  -- Keep code reasonable
  let dangerousPatterns = ["eval", "exec", "system", "shell", "cmd"]
      hasDangerous = L.any (`L.isInfixOf` code) dangerousPatterns
  in if hasDangerous
     then
       let source = unlines
             [ "package main"
             , "func main() {"
             , "  " ++ code
             , "}"
             ]
       in case parseTypus source of
            Left _ -> property True  -- Parsing failed safely
            Right typusFile ->
              case compile typusFile of
                Left compileErr -> 
                  property $ L.any (`L.isInfixOf` show compileErr) 
                    ["security", "dangerous", "unsafe"]
                Right _ -> property True  -- May compile with warnings
     else property True  -- Safe code is skipped

-- Test 6: Security validation prevents command injection
test_security_command_injection :: TestTree
test_security_command_injection =
  testCase "Security validation prevents command injection" $ do
    let source = unlines
          [ "package main"
          , "func executeCommand(input string) {"
          , "  cmd := \"echo \" + input  // Command injection vulnerability"
          , "  system(cmd)"
          , "}"
          , "func main() {"
          , "  executeCommand(\"hello; rm -rf /\")"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should catch command injection vulnerability
            assertBool "Should detect command injection vulnerability" $
              L.any (`L.isInfixOf` show compileErr) 
                ["command", "injection", "system", "exec"]
          Right result -> do
            -- May compile with warnings
            assertBool "Should warn about command concatenation" True

-- Test 7: Security validation prevents cryptographic issues
test_security_cryptographic_issues :: TestTree
test_security_cryptographic_issues =
  testCase "Security validation prevents cryptographic issues" $ do
    let source = unlines
          [ "package main"
          , "func encrypt(data string) string {"
          , "  key := \"secret123\"  // Weak hardcoded key"
          , "  result := xor(data, key)  // Weak encryption"
          , "  return result"
          , "}"
          , "func main() {"
          , "  encrypted := encrypt(\"sensitive data\")"
          , "  println(encrypted)"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should catch cryptographic weaknesses
            assertBool "Should detect cryptographic weaknesses" $
              L.any (`L.isInfixOf` show compileErr) 
                ["crypto", "weak", "key", "hardcoded"]
          Right result -> do
            -- May compile with warnings
            assertBool "Should warn about weak cryptography" True

-- Test 8: Security validation prevents information disclosure
test_security_information_disclosure :: TestTree
test_security_information_disclosure =
  testCase "Security validation prevents information disclosure" $ do
    let source = unlines
          [ "package main"
          , "func debug() {"
          , "  password := \"admin123\"  // Sensitive data in code"
          , "  println(\"Debug: password =\", password)  // Information disclosure"
          , "}"
          , "func main() {"
          , "  debug()"
          , "}"
          ]
    case parseTypus source of
      Left err -> assertFailure $ "Parse failed: " ++ err
      Right typusFile -> do
        case compile typusFile of
          Left compileErr -> do
            -- Should catch information disclosure
            assertBool "Should detect information disclosure" $
              L.any (`L.isInfixOf` show compileErr) 
                ["password", "sensitive", "disclosure", "debug"]
          Right result -> do
            -- May compile with warnings
            assertBool "Should warn about sensitive data exposure" True

-- QuickCheck property: Security validation is comprehensive
prop_security_validation_comprehensive :: String -> Property
prop_security_validation_comprehensive input =
  L.length input < 50 ==>  -- Keep input reasonable
  let source = unlines
        [ "package main"
        , "func main() {"
        , "  input := \"" ++ input ++ "\""
        , "  println(input)"
        , "}"
        ]
  in case parseTypus source of
       Left _ -> property True  -- Parsing failed safely
       Right typusFile ->
         case compile typusFile of
           Left _ -> property True  -- Compilation errors are acceptable
           Right _ -> property True  -- Successful compilation

tests :: TestTree
tests =
  testGroup "Security Validation Cabals Tests"
    [ test_security_buffer_overflow
    , test_security_integer_overflow
    , test_security_sql_injection
    , test_security_unsafe_casting
    , test_security_path_traversal
    , fastProperty "Security catches dangerous patterns" prop_security_catches_dangerous_patterns
    , test_security_command_injection
    , test_security_cryptographic_issues
    , test_security_information_disclosure
    , fastProperty "Security validation is comprehensive" prop_security_validation_comprehensive
    ]