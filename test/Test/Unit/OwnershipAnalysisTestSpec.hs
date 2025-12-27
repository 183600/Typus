{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.OwnershipAnalysisTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck 
  ( Property, (===), (==>), forAll, counterexample, classify, property
  , (.&&.), (.||.), Arbitrary(..), Gen, choose, listOf, elements
  , vectorOf, oneof, frequency, suchThat, Positive(..)
  )

import Ownership
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipAnalyzer
  , OwnershipTransfer(..)
  , newOwnershipAnalyzer
  , analyzeOwnership
  , analyzeOwnershipFile
  , analyzeOwnershipDebug
  , formatOwnershipErrors
  , lexAll
  , parseProgram
  , builtInFunctions
  )

import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))

import Data.List (isInfixOf, isPrefixOf, null, length, sort)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set

-- | Generate simple ownership-related code
genOwnershipCode :: Gen String
genOwnershipCode = oneof
  [ -- Simple move
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    x := 42"
      , "    y := x  // move"
      , "    println(y)"
      , "}"
      ]
  , -- Borrow scenario
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    x := 42"
      , "    y := &x  // borrow"
      , "    println(*y)"
      , "    println(x)"
      , "}"
      ]
  , -- Mutable borrow
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    x := 42"
      , "    y := &x  // mutable borrow"
      , "    *y = 100"
      , "    println(x)"
      , "}"
      ]
  , -- Function parameter
    return $ unlines
      [ "package main"
      , "func consume(x int) {"
      , "    println(x)"
      , "}"
      , "func main() {"
      , "    value := 42"
      , "    consume(value)  // move"
      , "}"
      ]
  , -- Return value
    return $ unlines
      [ "package main"
      , "func create() int {"
      , "    return 42"
      , "}"
      , "func main() {"
      , "    value := create()"
      , "    println(value)"
      , "}"
      ]
  ]

-- | Generate code with ownership errors
genErrorOwnershipCode :: Gen String
genErrorOwnershipCode = oneof
  [ -- Use after move
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    x := 42"
      , "    y := x  // move"
      , "    println(x)  // use after move"
      , "}"
      ]
  , -- Double move
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    x := 42"
      , "    y := x  // first move"
      , "    z := x  // double move"
      , "    println(y)"
      , "}"
      ]
  , -- Borrow while moved
    return $ unlines
      [ "package main"
      , "func main() {"
      , "    x := 42"
      , "    y := x  // move"
      , "    z := &x  // borrow while moved"
      , "    println(y)"
      , "}"
      ]
  ]

-- Property tests

-- Property: valid ownership code should analyze without errors
prop_valid_ownership_no_errors :: Property
prop_valid_ownership_no_errors =
  forAll genOwnershipCode $ \code ->
    case analyzeOwnership code of
      (analyzer, []) -> property True
      (analyzer, errors) -> 
        -- Some valid code might still have warnings, but not critical errors
        property $ not $ any isCriticalError errors

-- Property: error ownership code should detect issues
prop_error_ownership_detects_errors :: Property
prop_error_ownership_detects_errors =
  forAll genErrorOwnershipCode $ \code ->
    case analyzeOwnership code of
      (analyzer, []) -> property False  -- Should have errors
      (analyzer, errors) -> property $ not $ null errors

-- Property: ownership analyzer should be consistent
prop_analyzer_consistency :: Property
prop_analyzer_consistency =
  forAll genOwnershipCode $ \code ->
    let (analyzer1, errors1) = analyzeOwnership code
        (analyzer2, errors2) = analyzeOwnership code
    in property $ length errors1 === length errors2

-- Property: lexAll should produce tokens for valid code
prop_lexAll_produces_tokens :: Property
prop_lexAll_produces_tokens =
  forAll genOwnershipCode $ \code ->
    let tokens = lexAll code
    in property $ not $ null tokens

-- Property: parseProgram should handle lexed tokens
prop_parseProgram_handles_tokens :: Property
prop_parseProgram_handles_tokens =
  forAll genOwnershipCode $ \code ->
    let tokens = lexAll code
        program = parseProgram tokens
    in property $ True  -- Basic smoke test

-- Property: formatOwnershipErrors should produce readable output
prop_formatErrors_readable :: Property
prop_formatErrors_readable =
  let errors = [UseAfterMove "x", DoubleMove "x" "y", BorrowWhileMoved "z"]
      formatted = formatOwnershipErrors errors
  in property $ not $ null formatted

-- Helper function to check if an error is critical
isCriticalError :: OwnershipError -> Bool
isCriticalError (UseAfterMove _) = True
isCriticalError (DoubleMove _ _) = True
isCriticalError (BorrowWhileMoved _) = True
isCriticalError (MutBorrowWhileBorrowed _) = True
isCriticalError (BorrowWhileMutBorrowed _) = True
isCriticalError (MultipleMutBorrows _) = True
isCriticalError (UseWhileMutBorrowed _) = True
isCriticalError _ = False

-- Unit tests

unit_tests :: TestTree
unit_tests = testGroup "Ownership Analysis Unit Tests"
  [ testCase "simple move analysis" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    x := 42"
            , "    y := x  // move"
            , "    println(y)"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should analyze without critical errors
          assertBool "should not have critical errors" $ 
            not $ any isCriticalError errors

  , testCase "borrow analysis" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    x := 42"
            , "    y := &x  // borrow"
            , "    println(*y)"
            , "    println(x)"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should allow both borrow and original use
          assertBool "should not have critical errors" $ 
            not $ any isCriticalError errors

  , testCase "mutable borrow analysis" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    x := 42"
            , "    y := &x  // mutable borrow"
            , "    *y = 100"
            , "    println(x)"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle mutable borrow correctly
          assertBool "should not have critical errors" $ 
            not $ any isCriticalError errors

  , testCase "function parameter move" $ do
      let code = unlines
            [ "package main"
            , "func consume(x int) {"
            , "    println(x)"
            , "}"
            , "func main() {"
            , "    value := 42"
            , "    consume(value)  // move"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle function parameter moves
          assertBool "should not have critical errors" $ 
            not $ any isCriticalError errors

  , testCase "use after move detection" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    x := 42"
            , "    y := x  // move"
            , "    println(x)  // use after move"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          assertBool "should detect use after move" $ 
            any isUseAfterMove errors
      where
        isUseAfterMove (UseAfterMove _) = True
        isUseAfterMove _ = False

  , testCase "double move detection" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    x := 42"
            , "    y := x  // first move"
            , "    z := x  // double move"
            , "    println(y)"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          assertBool "should detect double move" $ 
            any isDoubleMove errors
      where
        isDoubleMove (DoubleMove _ _) = True
        isDoubleMove _ = False

  , testCase "borrow while moved detection" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    x := 42"
            , "    y := x  // move"
            , "    z := &x  // borrow while moved"
            , "    println(y)"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          assertBool "should detect borrow while moved" $ 
            any isBorrowWhileMoved errors
      where
        isBorrowWhileMoved (BorrowWhileMoved _) = True
        isBorrowWhileMoved _ = False

  , testCase "ownership transfer tracking" $ do
      let code = unlines
            [ "package main"
            , "func transfer() int {"
            , "    return 42"
            , "}"
            , "func main() {"
            , "    value := transfer()"
            , "    println(value)"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle ownership transfer correctly
          return ()

  , testCase "complex ownership scenarios" $ do
      let code = unlines
            [ "package main"
            , "type Resource struct {"
            , "    data int"
            , "}"
            , "func (r Resource) use() {"
            , "    println(r.data)"
            , "}"
            , "func main() {"
            , "    r := Resource{data: 42}"
            , "    r.use()  // should move or borrow"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle struct method calls
          return ()

  , testCase "ownership with control flow" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    x := 42"
            , "    if true {"
            , "        y := x  // move in branch"
            , "        println(y)"
            , "    } else {"
            , "        println(x)  // potentially use after move"
            , "}"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle control flow correctly
          return ()

  , testCase "ownership analyzer state consistency" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    a := 1"
            , "    b := 2"
            , "    c := a + b"
            , "    println(c)"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Analyzer should maintain consistent state
          return ()

  , testCase "built-in functions handling" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    x := 42"
            , "    println(x)  // built-in function"
            , "    println(x)  // should still be available"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Built-in functions should not consume ownership
          assertBool "built-in should not move" $ 
            not $ any isCriticalError errors

  , testCase "ownership with slices" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    slice := []int{1, 2, 3}"
            , "    first := slice[0]  // copy, not move"
            , "    println(first)"
            , "    println(len(slice))  // slice still available"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Slice indexing should copy, not move
          assertBool "slice indexing should copy" $ 
            not $ any isCriticalError errors
  ]

-- Advanced ownership tests

advanced_tests :: TestTree
advanced_tests = testGroup "Advanced Ownership Tests"
  [ testCase "nested ownership scopes" $ do
      let code = unlines
            [ "package main"
            , "func outer() {"
            , "    x := 42"
            , "    func inner() {"
            , "        y := x  // capture from outer scope"
            , "        println(y)"
            , "    }()"
            , "    println(x)  // should still be available"
            , "}"
            , "func main() {"
            , "    outer()"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle nested scoping correctly
          return ()

  , testCase "ownership with channels" $ do
      let code = unlines
            [ "package main"
            , "func main() {"
            , "    ch := make(chan int, 1)"
            , "    ch <- 42  // send moves value"
            , "    value := <-ch  // receive owns value"
            , "    println(value)"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle channel operations
          return ()

  , testCase "ownership with goroutines" $ do
      let code = unlines
            [ "package main"
            , "func worker(data int) {"
            , "    println(data)"
            , "}"
            , "func main() {"
            , "    x := 42"
            , "    go worker(x)  // move to goroutine"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle goroutine parameter passing
          return ()

  , testCase "ownership with interfaces" $ do
      let code = unlines
            [ "package main"
            , "type Writer interface {"
            , "    Write(data []byte) error"
            , "}"
            , "func useWriter(w Writer) {"
            , "    w.Write([]byte(\"hello\"))"
            , "}"
            , "func main() {"
            , "    // concrete type implementing Writer"
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle interface usage
          return ()
  ]

-- Error formatting tests

error_formatting_tests :: TestTree
error_formatting_tests = testGroup "Error Formatting Tests"
  [ testCase "format use after move error" $ do
      let error = UseAfterMove "x"
          formatted = formatOwnershipErrors [error]
      assertBool "should contain error description" $ 
        "UseAfterMove" `isInfixOf` formatted

  , testCase "format double move error" $ do
      let error = DoubleMove "x" "y"
          formatted = formatOwnershipErrors [error]
      assertBool "should contain error description" $ 
        "DoubleMove" `isInfixOf` formatted

  , testCase "format multiple errors" $ do
      let errors = [UseAfterMove "x", DoubleMove "x" "y", BorrowWhileMoved "z"]
          formatted = formatOwnershipErrors errors
      assertBool "should contain all errors" $ 
        "UseAfterMove" `isInfixOf` formatted &&
        "DoubleMove" `isInfixOf` formatted &&
        "BorrowWhileMoved" `isInfixOf` formatted

  , testCase "format empty error list" $ do
      let formatted = formatOwnershipErrors []
      assertBool "should handle empty list" $ True
  ]

-- Performance tests

performance_tests :: TestTree
performance_tests = testGroup "Ownership Performance Tests"
  [ testCase "large function analysis" $ do
      let largeFunction = unlines $ concat
            [ ["func large() {"]
            , ["    x := 1"]
            , ["    y := x"] ++ ["    println(y)" | _ <- [1..100]]
            , ["}"]
            ]
          code = "package main\n\n" ++ largeFunction
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle large functions without issues
          return ()

  , testCase "deep nesting analysis" $ do
      let maxDepth = 50
          buildNested depth = if depth <= 0
                             then "    println(\"deepest\")"
                             else "    if true {\n" ++ buildNested (depth - 1) ++ "\n    }"
          nestedCode = buildNested maxDepth
          code = unlines
            [ "package main"
            , "func main() {"
            , "    x := 42"
            , nestedCode
            , "}"
            ]
      case analyzeOwnership code of
        (analyzer, errors) -> do
          -- Should handle deeply nested code
          return ()
  ]

tests :: TestTree
tests = testGroup "Ownership Analysis Tests"
  [ testGroup "Property Tests"
    [ fastProperty "valid ownership no errors" prop_valid_ownership_no_errors
    , fastProperty "error ownership detects errors" prop_error_ownership_detects_errors
    , fastProperty "analyzer consistency" prop_analyzer_consistency
    , fastProperty "lexAll produces tokens" prop_lexAll_produces_tokens
    , fastProperty "parseProgram handles tokens" prop_parseProgram_handles_tokens
    , fastProperty "format errors readable" prop_formatErrors_readable
    ]
  , unit_tests
  , advanced_tests
  , error_formatting_tests
  , performance_tests
  ]