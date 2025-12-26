{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.NewCabalParserQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool, assertFailure)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Arbitrary(..), Gen, choose, listOf, oneof, elements, suchThat, frequency)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedValue
  , spanStart
  , spanEnd
  )

-- ============================================================================
-- Arbitrary instances for Parser testing
-- ============================================================================

-- Generate valid directive keys
genDirectiveKey :: Gen String
genDirectiveKey = elements ["ownership", "dependent_types", "constraints"]

-- Generate valid directive values
genDirectiveValue :: Gen String
genDirectiveValue = elements ["on", "off", "true", "false"]

-- Generate file directive lines
genFileDirectiveLine :: Gen String
genFileDirectiveLine = do
  key <- genDirectiveKey
  value <- genDirectiveValue
  return $ "//! " ++ key ++ ": " ++ value

-- Generate multiple file directive lines
genFileDirectiveLines :: Gen String
genFileDirectiveLines = do
  count <- choose (1, 3)
  directives <- listOf count genFileDirectiveLine
  return $ unlines directives

-- Generate build tag lines
genBuildTagLine :: Gen String
genBuildTagLine = oneof
  [ do
      tag <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ '_'
      return $ "//go:build " ++ tag
  , do
      tags <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ '_'
      return $ "// +build " ++ unwords tags
  ]

-- Generate Go package declaration
genPackageDecl :: Gen String
genPackageDecl = do
  name <- elements ["main", "lib", "utils", "core", "types"]
  return $ "package " ++ name

-- Generate simple Go function
genSimpleFunction :: Gen String
genSimpleFunction = do
  name <- listOf $ elements $ ['a'..'z']
  return $ "func " ++ name ++ "() {\n    // body\n}"

-- Generate Go code block
genGoCodeBlock :: Gen String
genGoCodeBlock = do
  linesCount <- choose (1, 5)
  lines' <- sequence $ replicate linesCount $ oneof
    [ pure "    fmt.Println(\"test\")"
    , pure "    x := 42"
    , pure "    return x"
    , pure "    // comment"
    , genSimpleFunction
    ]
  return $ unlines lines'

-- Generate block directive line
genBlockDirectiveLine :: Gen String
genBlockDirectiveLine = do
  count <- choose (1, 3)
  directives <- sequence $ replicate count $ do
    key <- genDirectiveKey
    value <- genDirectiveValue
    return $ key ++ ": " ++ value
  let directivesStr = unwords directives
  return $ "{//! " ++ directivesStr ++ "}"

-- Generate complete Typus file content
genTypusFileContent :: Gen String
genTypusFileContent = do
  hasFileDirectives <- elements [True, False]
  hasBuildTags <- elements [True, False]
  hasBlockDirectives <- elements [True, False]
  
  parts <- sequence $
    [ if hasFileDirectives then Just <$> genFileDirectiveLines else pure Nothing
    , if hasBuildTags then Just <$> genBuildTagLine else pure Nothing
    , Just <$> genPackageDecl
    , Just <$> genSimpleFunction
    , if hasBlockDirectives then Just <$> genBlockDirectiveLine else pure Nothing
    , if hasBlockDirectives then Just <$> genGoCodeBlock else pure Nothing
    , Just <$> pure "}"
    ]
  
  return $ unlines $ filter (not . null) parts

-- Generate malformed directive lines for error testing
genMalformedDirectiveLine :: Gen String
genMalformedDirectiveLine = oneof
  [ do
      key <- listOf $ elements $ ['a'..'z']
      return $ "//! " ++ key ++ " " ++ key  -- Missing colon
  , do
      key <- genDirectiveKey
      value <- genDirectiveValue
      return $ "//!" ++ key ++ ": " ++ value  -- Missing space after //!
  , do
      key <- elements ["invalid", "unknown", "bad"]
      value <- genDirectiveValue
      return $ "//! " ++ key ++ ": " ++ value  -- Invalid key
  ]

-- ============================================================================
-- Property Tests for Parser
-- ============================================================================

-- Basic parsing properties
prop_parseTypusHandlesEmptyInput :: Property
prop_parseTypusHandlesEmptyInput =
  let result = parseTypus ""
  in case result of
    Left _ -> False
    Right file -> tfDirectives file == defaultFileDirectives &&
                  null (tfBuildTags file) &&
                  null (tfBlocks file)

prop_parseTypusHandlesOnlyWhitespace :: String -> Property
prop_parseTypusHandlesOnlyWhitespace ws =
  all (`elem`" \t\n\r") ws ==>
    let result = parseTypus ws
    in case result of
      Left _ -> False
      Right file -> tfDirectives file == defaultFileDirectives &&
                    null (tfBuildTags file) &&
                    null (tfBlocks file)

prop_parseTypusPreservesPackageDecl :: String -> Property
prop_parseTypusPreservesPackageDecl packageName =
  not (null packageName) && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_') packageName ==>
    let input = "package " ++ packageName ++ "\nfunc main() {}"
        result = parseTypus input
    in case result of
      Left _ -> False
      Right file -> any (isInfixOf ("package " ++ packageName)) (map cbContent (tfBlocks file))

prop_parseTypusHandlesFileDirectives :: String -> Property
prop_parseTypusHandlesFileDirectives directiveLine =
  "//!" `isPrefixOf` directiveLine ==>
    let input = directiveLine ++ "\npackage main\nfunc main() {}"
        result = parseTypus input
    in case result of
      Left _ -> False
      Right file -> tfDirectives file /= defaultFileDirectives

prop_parseTypusHandlesBuildTags :: String -> Property
prop_parseTypusHandlesBuildTags buildTagLine =
  ("//go:build" `isPrefixOf` buildTagLine || "// +build" `isPrefixOf` buildTagLine) ==>
    let input = buildTagLine ++ "\npackage main\nfunc main() {}"
        result = parseTypus input
    in case result of
      Left _ -> False
      Right file -> not (null (tfBuildTags file))

prop_parseTypusHandlesBlockDirectives :: String -> Property
prop_parseTypusHandlesBlockDirectives blockDirective =
  "{//!" `isPrefixOf` blockDirective ==>
    let input = "package main\nfunc main() {\n" ++ blockDirective ++ "\n    // body\n}"
        result = parseTypus input
    in case result of
      Left _ -> False
      Right file -> any (hasBlockDirective (tfBlocks file)) (tfBlocks file)
  where
    hasBlockDirective blocks = any (\block -> cbDirectives block /= defaultBlockDirectives) blocks

-- Error handling properties
prop_parseTypusDetectsMultiplePackageDecls :: String -> Property
prop_parseTypusDetectsMultiplePackageDecls packageName =
  not (null packageName) && all (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ '_') packageName ==>
    let input = "package " ++ packageName ++ "\npackage main\nfunc main() {}"
        result = parseTypus input
    in case result of
      Left err -> "Multiple package declarations" `isInfixOf` err
      Right _ -> False

prop_parseTypusDetectsUnknownDirectives :: String -> Property
prop_parseTypusDetectsUnknownDirectives directiveLine =
  "//!" `isPrefixOf` directiveLine && "unknown" `isInfixOf` directiveLine ==>
    let input = directiveLine ++ "\npackage main\nfunc main() {}"
        result = parseTypus input
    in case result of
      Left err -> "Unknown file directive" `isInfixOf` err
      Right _ -> False

prop_parseTypusDetectsUnclosedBlockDirectives :: String -> Property
prop_parseTypusDetectsUnclosedBlockDirectives blockDirective =
  "{//!" `isPrefixOf` blockDirective ==>
    let input = "package main\nfunc main() {\n" ++ blockDirective ++ "\n    // body\n}"  -- Missing closing }
        result = parseTypus input
    in case result of
      Left err -> "Unclosed directive block" `isInfixOf` err
      Right _ -> False

-- Directive parsing properties
prop_parseTypusHandlesConstraintsDirective :: Property
prop_parseTypusHandlesConstraintsDirective =
  let input = "//! constraints: on\npackage main\nfunc main() {}"
      result = parseTypus input
  in case result of
    Left _ -> False
    Right file -> 
      let dirs = tfDirectives file
      in isJust (fdConstraints dirs) && 
         isJust (fdDependentTypes dirs) &&
         locatedValue (fromMaybe (error "impossible") (fdDependentTypes dirs)) == True

prop_parseTypusHandlesOwnershipDirective :: String -> Property
prop_parseTypusHandlesOwnershipDirective value =
  value `elem` ["on", "off"] ==>
    let input = "//! ownership: " ++ value ++ "\npackage main\nfunc main() {}"
        result = parseTypus input
    in case result of
      Left _ -> False
      Right file -> 
        let dirs = tfDirectives file
            expectedValue = value == "on"
        in case fdOwnership dirs of
          Nothing -> False
          Just loc -> locatedValue loc == expectedValue

-- ============================================================================
-- Unit Tests for Edge Cases
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser QuickCheck Tests"
  [ testGroup "Basic parsing properties"
    [ fastProperty "parseTypus handles empty input" prop_parseTypusHandlesEmptyInput
    , fastProperty "parseTypus handles only whitespace" prop_parseTypusHandlesOnlyWhitespace
    , fastProperty "parseTypus preserves package declaration" prop_parseTypusPreservesPackageDecl
    , fastProperty "parseTypus handles file directives" prop_parseTypusHandlesFileDirectives
    , fastProperty "parseTypus handles build tags" prop_parseTypusHandlesBuildTags
    , fastProperty "parseTypus handles block directives" prop_parseTypusHandlesBlockDirectives
    , testCase "parseTypus handles simple valid input" $ do
        let input = unlines
              [ "package main"
              , "func main() {"
              , "    fmt.Println(\"Hello, World!\")"
              , "}"
              ]
        case parseTypus input of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right file -> do
            tfDirectives file @?= defaultFileDirectives
            assertBool "should have blocks" $ not (null (tfBlocks file))

    , testCase "parseTypus handles complex file with multiple directives" $ do
        let input = unlines
              [ "//! ownership: on, dependent_types: off"
              , "//go:build linux"
              , "package main"
              , "func main() {"
              , "    {//! ownership: off, dependent_types: on}"
              , "        fmt.Println(\"test\")"
              , "    }"
              , "}"
              ]
        case parseTypus input of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right file -> do
            let dirs = tfDirectives file
            case fdOwnership dirs of
              Nothing -> assertFailure "expected ownership directive"
              Just loc -> locatedValue loc @?= True
            case fdDependentTypes dirs of
              Nothing -> assertFailure "expected dependent_types directive"
              Just loc -> locatedValue loc @?= False
            assertBool "should have build tags" $ not (null (tfBuildTags file))
            assertBool "should have blocks with directives" $ any (\block -> 
              case bdOwnership (cbDirectives block) of
                Nothing -> False
                Just loc -> not (locatedValue loc)
              ) (tfBlocks file)
    ]

  , testGroup "Error handling properties"
    [ fastProperty "parseTypus detects multiple package declarations" prop_parseTypusDetectsMultiplePackageDecls
    , fastProperty "parseTypus detects unknown directives" prop_parseTypusDetectsUnknownDirectives
    , fastProperty "parseTypus detects unclosed block directives" prop_parseTypusDetectsUnclosedBlockDirectives
    , testCase "parseTypus handles malformed file directive" $ do
        let input = "//! invalid_directive\npackage main\nfunc main() {}"
        case parseTypus input of
          Left err -> assertBool "error should mention unknown directive" $ "Unknown file directive" `isInfixOf` err
          Right _ -> assertFailure "expected parse failure for unknown directive"

    , testCase "parseTypus handles malformed block directive" $ do
        let input = "package main\nfunc main() {\n{//! ownership}\n    fmt.Println(\"test\")\n}"
        case parseTypus input of
          Left err -> assertBool "error should mention invalid format" $ "Invalid block directive" `isInfixOf` err
          Right _ -> assertFailure "expected parse failure for malformed directive"

    , testCase "parseTypus handles if statement without opening brace" $ do
        let input = "package main\nfunc main() {\nif x > 0\n    fmt.Println(\"test\")\n}"
        case parseTypus input of
          Left err -> assertBool "error should mention missing opening brace" $ "missing opening brace" `isInfixOf` err
          Right _ -> assertFailure "expected parse failure for if without brace"
    ]

  , testGroup "Directive parsing properties"
    [ fastProperty "parseTypus handles constraints directive" prop_parseTypusHandlesConstraintsDirective
    , fastProperty "parseTypus handles ownership directive" prop_parseTypusHandlesOwnershipDirective
    , testCase "parseTypus handles boolean value variations" $ do
        let inputs = 
              [ ("//! ownership: true", True)
              , ("//! ownership: false", False)
              , ("//! ownership: on", True)
              , ("//! ownership: off", False)
              ]
        sequence_ $ map (\(directive, expected) -> do
          let input = directive ++ "\npackage main\nfunc main() {}"
          case parseTypus input of
            Left err -> assertFailure $ "parseTypus failed for " ++ directive ++ ": " ++ err
            Right file -> 
              case fdOwnership (tfDirectives file) of
                Nothing -> assertFailure $ "expected ownership directive for " ++ directive
                Just loc -> locatedValue loc @?= expected
          ) inputs

    , testCase "parseTypus handles mixed directive formats" $ do
        let input = unlines
              [ "//! ownership: on, dependent_types: true"
              , "package main"
              , "func main() {"
              , "    {//! constraints: off, ownership: false}"
              , "        fmt.Println(\"test\")"
              , "    }"
              , "}"
              ]
        case parseTypus input of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right file -> do
            let fileDirs = tfDirectives file
            case fdOwnership fileDirs of
              Nothing -> assertFailure "expected file ownership directive"
              Just loc -> locatedValue loc @?= True
            case fdDependentTypes fileDirs of
              Nothing -> assertFailure "expected file dependent_types directive"
              Just loc -> locatedValue loc @?= True
            
            let blocks = tfBlocks file
                ownershipBlock = filter (\block -> 
                  case bdOwnership (cbDirectives block) of
                    Nothing -> False
                    Just loc -> not (locatedValue loc)
                  ) blocks
            assertBool "should have block with ownership: false" $ not (null ownershipBlock)
    ]

  , testGroup "Edge case tests"
    [ testCase "parseTypus handles file with only directives" $ do
        let input = unlines
              [ "//! ownership: on"
              , "//! dependent_types: off"
              , "//go:build linux"
              ]
        case parseTypus input of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right file -> do
            let dirs = tfDirectives file
            case fdOwnership dirs of
              Nothing -> assertFailure "expected ownership directive"
              Just loc -> locatedValue loc @?= True
            assertBool "should have build tags" $ not (null (tfBuildTags file))

    , testCase "parseTypus handles file with only comments" $ do
        let input = unlines
              [ "// This is a comment"
              , "/* This is a block comment */"
              , "// Another comment"
              ]
        case parseTypus input of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right file -> do
            tfDirectives file @?= defaultFileDirectives
            tfBuildTags file @?= []
            tfBlocks file @?= []

    , testCase "parseTypus handles file with empty lines" $ do
        let input = unlines
              [ ""
              , "   "
              , "\t"
              , "package main"
              , ""
              , "func main() {}"
              , ""
              ]
        case parseTypus input of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right file -> do
            assertBool "should parse successfully" $ True

    , testCase "parseTypus handles block directive without closing brace in same line" $ do
        let input = unlines
              [ "package main"
              , "func main() {"
              , "    {//! ownership: on"
              , "        fmt.Println(\"test\")"
              , "    }"
              , "}"
              ]
        case parseTypus input of
          Left err -> assertFailure $ "parseTypus failed: " ++ err
          Right file -> do
            let blocks = tfBlocks file
                ownershipBlock = filter (\block -> 
                  case bdOwnership (cbDirectives block) of
                    Nothing -> False
                    Just loc -> locatedValue loc
                  ) blocks
            assertBool "should have block with ownership: on" $ not (null ownershipBlock)
    ]
  ]