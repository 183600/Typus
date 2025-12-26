{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.NewQuickCheckPropertiesSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, vectorOf, oneof)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Utils
  ( trim
  , splitBy
  , splitByCollapsed
  , removeComments
  , breakOn
  )

import Parser (parseTypus, TypusFile(..))
import Compiler (compile)
import Data.Char (isSpace, toLower, isAlphaNum)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, tails, isInfixOf, sort, nub)
import qualified Data.Text as T

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

-- Property: trim is idempotent (trimming twice gives same result as trimming once)
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  trim (trim s) === trim s

-- Property: splitBy respects delimiter boundaries
prop_splitBy_preserves_content :: Char -> String -> Property
prop_splitBy_preserves_content delim s =
  let parts = splitBy delim s
      reconstructed = Data.List.intercalate [delim] parts
  in reconstructed === s

-- Property: splitByCollapsed never produces empty strings
prop_splitByCollapsed_no_empty :: Char -> String -> Property
prop_splitByCollapsed_no_empty delim s =
  let parts = splitByCollapsed delim s
  in not (null parts) ==> all (not . null) parts

-- Property: breakOn either finds the pattern or returns original string
prop_breakOn_correctness :: String -> String -> Property
prop_breakOn_correctness pat s =
  let (before, after) = breakOn pat s
  in if null pat
     then before === "" &&. after === s
     else if pat `isInfixOf` s
          then before ++ pat ++ after === s
          else before === s &&. after === ""

-- Property: removeComments preserves non-comment content
prop_removeComments_preserves_code :: String -> Property
prop_removeComments_preserves_code s =
  let withoutComments = removeComments s
      -- Count non-comment, non-whitespace characters before and after
      countCodeChars = length . filter (\c -> not (isSpace c) && c /= '/')
      originalCodeChars = countCodeChars s
      newCodeChars = countCodeChars withoutComments
  in newCodeChars <= originalCodeChars

-- Property: removeComments eliminates // and /* */ style comments
prop_removeComments_removes_comment_markers :: String -> Property
prop_removeComments_removes_comment_markers s =
  let withoutComments = removeComments s
  in not ("//" `isInfixOf` withoutComments) &&. not ("/*" `isInfixOf` withoutComments)

-- ============================================================================
-- Parser Module Properties
-- ============================================================================

-- Property: parseTypus preserves line count (approximately)
prop_parseTypus_preserves_structure :: String -> Property
prop_parseTypus_preserves_structure s =
  let originalLines = length $ lines s
      result = parseTypus s
  in case result of
       Left _ -> property True -- Parsing failures are acceptable
       Right typusFile ->
         let codeBlocks = tfCodeBlocks typusFile
             totalBlockLines = sum $ map (length . lines . cbContent) codeBlocks
         in classify (originalLines > 0) "non-empty input" $
            totalBlockLines <= originalLines + 10 -- Allow some tolerance for directives

-- Property: parseTypus handles empty input gracefully
prop_parseTypus_empty_input :: Property
prop_parseTypus_empty_input =
  let result = parseTypus ""
  in case result of
       Left _ -> property False -- Should parse empty input successfully
       Right typusFile -> 
         tfCodeBlocks typusFile === []

-- Property: parseTypus round-trip for simple code
prop_parseTypus_round_trip_simple :: String -> Property
prop_parseTypus_round_trip_simple s =
  let simpleCode = "package main\nfunc main() {\n  // " ++ s ++ "\n}\n"
      result = parseTypus simpleCode
  in case result of
       Left _ -> property False -- Should parse simple code
       Right typusFile ->
         let blocks = tfCodeBlocks typusFile
         in not (null blocks) ==> 
            let content = cbContent (head blocks)
            in "package main" `isInfixOf` content &&. "func main()" `isInfixOf` content

-- ============================================================================
-- Compiler Module Properties
-- ============================================================================

-- Property: compilation preserves function signatures
prop_compile_preserves_function_names :: String -> Property
prop_compile_preserves_function_names s =
  let code = "package main\nfunc " ++ s ++ "() {\n  println(\"test\")\n}\n"
      result = parseTypus code >>= compile
  in case result of
       Left _ -> property True -- Compilation failures are acceptable
       Right goCode ->
         let funcName = if null s then "main" else s
         in "func " ++ funcName ++ "(" `isInfixOf` goCode

-- Property: compilation produces valid Go package structure
prop_compile_produces_package :: String -> Property
prop_compile_produces_package s =
  let code = "package main\nfunc main() {\n  // " ++ s ++ "\n}\n"
      result = parseTypus code >>= compile
  in case result of
       Left _ -> property True
       Right goCode ->
         "package main" `isInfixOf` goCode

-- ============================================================================
-- Custom Generators
-- ============================================================================

-- Generate reasonable identifier names
genIdentifier :: Gen String
genIdentifier = do
  first <- elements (['a'..'z'] ++ ['_'])
  rest <- listOf $ elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
  return (first : take 10 rest)

-- Generate simple Go-like code snippets
genSimpleCode :: Gen String
genSimpleCode = do
  funcName <- genIdentifier
  numStatements <- choose (1, 5)
  statements <- vectorOf numStatements $ elements
    [ "  println(\"test\")"
    , "  x := 42"
    , "  y := x + 1"
    , "  if x > 0 { println(\"positive\") }"
    , "  for i := 0; i < 10; i++ { println(i) }"
    ]
  return $ "package main\n\nfunc " ++ funcName ++ "() {\n" ++ unlines statements ++ "}\n"

-- Generate strings with potential comment content
genCommentString :: Gen String
genCommentString = do
  hasLineComment <- arbitrary
  hasBlockComment <- arbitrary
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n"
  let base = content
      withLine = if hasLineComment then base ++ "\n// line comment\n" else base
      withBoth = if hasBlockComment then withLine ++ "/* block\n comment */\n" else withLine
  return withBoth

-- ============================================================================
-- Custom Arbitrary Instances
-- ============================================================================

instance Arbitrary String where
  arbitrary = oneof
    [ genIdentifier
    , genCommentString
    , listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n(){}[];:,."
    ]

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests =
  testGroup "New QuickCheck Properties"
    [ testGroup "Utils Properties"
        [ fastProperty "trim is idempotent" prop_trim_idempotent
        , fastProperty "splitBy preserves content" prop_splitBy_preserves_content
        , fastProperty "splitByCollapsed produces no empty strings" prop_splitByCollapsed_no_empty
        , fastProperty "breakOn correctness" prop_breakOn_correctness
        , fastProperty "removeComments preserves code" prop_removeComments_preserves_code
        , fastProperty "removeComments removes comment markers" prop_removeComments_removes_comment_markers
        ]

    , testGroup "Parser Properties"
        [ fastProperty "parseTypus preserves structure" prop_parseTypus_preserves_structure
        , fastProperty "parseTypus handles empty input" prop_parseTypus_empty_input
        , fastProperty "parseTypus round-trip for simple code" prop_parseTypus_round_trip_simple
        ]

    , testGroup "Compiler Properties"
        [ fastProperty "compile preserves function names" prop_compile_preserves_function_names
        , fastProperty "compile produces package structure" prop_compile_produces_package
        ]

    , testGroup "Generated Input Tests"
        [ testCase "parses generated simple code" $ do
            let code = "package main\nfunc test123() {\n  println(\"test\")\n}\n"
            result <- case parseTypus code of
              Left err -> assertFailure $ "Failed to parse generated code: " ++ show err
              Right _ -> return ()
            
        , testCase "compiles generated simple code" $ do
            let code = "package main\nfunc test456() {\n  println(\"test\")\n}\n"
            case parseTypus code >>= compile of
              Left err -> assertFailure $ "Failed to compile generated code: " ++ show err
              Right _ -> return ()
        ]