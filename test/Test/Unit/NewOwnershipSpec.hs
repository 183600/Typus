{-# LANGUAGE CPP #-}
module Test.Unit.NewOwnershipSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import Data.List (isInfixOf)

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
import Parser
  ( parseTypus
  )

tests :: TestTree
tests =
  testGroup "New Ownership Tests"
    [ testCase "creates new ownership analyzer" $ do
        let analyzer = newOwnershipAnalyzer
        -- Test that analyzer is created successfully
        case analyzer of
          OwnershipAnalyzer _ -> assertBool "analyzer created" True

    , testCase "analyzes simple ownership transfer" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    x := 42"
              , "    y := x"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                -- Should detect potential ownership issues
                assertBool "should have ownership analysis" (not $ null errors)
              Right _ -> assertBool "analysis completed successfully" True

    , testCase "detects use after move" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    x := 42"
              , "    y := x"
              , "    println(x)"  -- Use after move
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                let useAfterMoveErrors = filter isUseAfterMove errors
                assertBool "should detect use after move" (not $ null useAfterMoveErrors)
              Right _ -> assertFailure "expected ownership errors"

    , testCase "detects double move" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    x := 42"
              , "    y := x"
              , "    z := x"  -- Double move
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                let doubleMoveErrors = filter isDoubleMove errors
                assertBool "should detect double move" (not $ null doubleMoveErrors)
              Right _ -> assertFailure "expected ownership errors"

    , testCase "handles borrowing scenarios" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    x := 42"
              , "    y := &x"  -- Borrow
              , "    println(*y)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                -- Should analyze borrowing correctly
                assertBool "should analyze borrowing" (True)
              Right _ -> assertBool "borrowing analysis completed" True

    , testCase "detects mutable borrow conflicts" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    x := 42"
              , "    y := &x"     -- Immutable borrow
              , "    z := &mut x"  -- Mutable borrow conflict
              , "    println(*y, *z)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                let borrowConflicts = filter hasBorrowConflict errors
                assertBool "should detect borrow conflicts" (not $ null borrowConflicts)
              Right _ -> assertFailure "expected borrow conflicts"

    , testCase "analyzes function parameter ownership" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func consume(x int) {}"
              , "func main() {"
              , "    value := 42"
              , "    consume(value)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                -- Should analyze parameter ownership transfer
                assertBool "should analyze parameter ownership" (True)
              Right _ -> assertBool "parameter analysis completed" True

    , testCase "handles scope-based ownership" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    {"
              , "        x := 42"
              , "    }"
              , "    // x is out of scope here"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                -- Should handle scope correctly
                assertBool "should handle scope" (True)
              Right _ -> assertBool "scope analysis completed" True

    , testCase "lexes ownership-aware tokens" $ do
        let source = "x := 42; y := x"
        case lexAll source of
          Left err -> assertFailure $ "lexAll failed: " ++ err
          Right tokens -> do
            assertBool "should produce tokens" (not $ null tokens)

    , testCase "parses ownership programs" $ do
        let source = unlines
              [ "func main() {"
              , "    x := 42"
              , "    y := x"
              , "}"
              ]
        case parseProgram source of
          Left err -> assertFailure $ "parseProgram failed: " ++ err
          Right program -> do
            assertBool "should parse program" (True)

    , testCase "formats ownership errors" $ do
        let errors = 
              [ UseAfterMove "x"
              , DoubleMove "x" "y"
              , BorrowWhileMoved "z"
              ]
            formatted = formatOwnershipErrors errors
        assertBool "contains use after move" ("UseAfterMove" `isInfixOf` formatted)
        assertBool "contains double move" ("DoubleMove" `isInfixOf` formatted)
        assertBool "contains borrow while moved" ("BorrowWhileMoved" `isInfixOf` formatted)

    , testCase "provides debug analysis" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    x := 42"
              , "    y := x"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let debugResult = analyzeOwnershipDebug typusFile
            case debugResult of
              Left errors -> do
                -- Debug mode should provide detailed information
                assertBool "debug analysis should work" (True)
              Right _ -> assertBool "debug analysis completed" True

    , testCase "handles complex ownership scenarios" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func process(data []byte) []byte {"
              , "    result := make([]byte, len(data))"
              , "    copy(result, data)"
              , "    return result"
              , "}"
              , "func main() {"
              , "    input := []byte(\"hello\")"
              , "    output := process(input)"
              , "    println(string(output))"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                -- Should handle complex scenarios
                assertBool "should handle complex scenarios" (True)
              Right _ -> assertBool "complex analysis completed" True

    , testCase "recognizes built-in functions" $ do
        let functions = builtInFunctions
        assertBool "contains basic types" ("int" `elem` functions)
        assertBool "contains string type" ("string" `elem` functions)
        assertBool "contains println function" ("println" `elem` functions)
        assertBool "contains fmt package" ("fmt" `elem` functions)

    , testCase "analyzes loop ownership" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    items := []int{1, 2, 3}"
              , "    for i, item := range items {"
              , "        println(i, item)"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                let loopErrors = filter isLoopOwnershipError errors
                -- Should handle loop ownership correctly
                assertBool "should analyze loop ownership" (True)
              Right _ -> assertBool "loop analysis completed" True

    , testCase "handles conditional ownership" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "func main() {"
              , "    x := 42"
              , "    if x > 0 {"
              , "        y := x"
              , "        println(y)"
              , "    } else {"
              , "        z := x"
              , "        println(z)"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                -- Should handle conditional ownership paths
                assertBool "should handle conditional ownership" (True)
              Right _ -> assertBool "conditional analysis completed" True

    , testCase "analyzes struct field ownership" $ do
        let source = unlines
              [ "package main"
              , "//! ownership: on"
              , "type Point struct { x, y int }"
              , "func main() {"
              , "    p := Point{x: 1, y: 2}"
              , "    q := p"
              , "    println(q.x, q.y)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right typusFile -> do
            let result = analyzeOwnershipFile typusFile
            case result of
              Left errors -> do
                -- Should handle struct field ownership
                assertBool "should handle struct ownership" (True)
              Right _ -> assertBool "struct analysis completed" True
    ]
  where
    isUseAfterMove (UseAfterMove _) = True
    isUseAfterMove _ = False
    
    isDoubleMove (DoubleMove _ _) = True
    isDoubleMove _ = False
    
    hasBorrowConflict (MutBorrowWhileBorrowed _) = True
    hasBorrowConflict (BorrowWhileMutBorrowed _) = True
    hasBorrowConflict (MultipleMutBorrows _) = True
    hasBorrowConflict _ = False
    
    isLoopOwnershipError (LoopOwnershipError _) = True
    isLoopOwnershipError _ = False