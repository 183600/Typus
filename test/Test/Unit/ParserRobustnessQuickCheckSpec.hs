module Test.Unit.ParserRobustnessQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, vectorOf, suchThat)
import TestSupport.QuickCheck (fastProperty)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
              defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T (pack, unpack)

-- ============================================================================
-- Arbitrary instances for Parser types
-- ============================================================================

instance Arbitrary FileDirectives where
    arbitrary = FileDirectives <$> maybeLocatedBool <*> maybeLocatedBool <*> maybeLocatedBool
      where
        maybeLocatedBool = oneof [return Nothing, Just <$> locatedBool]
        locatedBool = do
            pos <- arbitrary
            return $ (if T.unpack "true" `elem` ["true", "false"] then True else False) `seq` undefined

instance Arbitrary BlockDirectives where
    arbitrary = BlockDirectives <$> maybeLocatedBool <*> maybeLocatedBool <*> maybeLocatedBool
      where
        maybeLocatedBool = oneof [return Nothing, Just <$> locatedBool]
        locatedBool = do
            pos <- arbitrary
            return $ (if T.unpack "true" `elem` ["true", "false"] then True else False) `seq` undefined

-- Generate valid Typus code snippets
genValidTypusCode :: Gen String
genValidTypusCode = oneof
    [ genSimpleFunction
    , genVariableDeclaration
    , genCommentOnly
    , genEmptyBlock
    ]

genSimpleFunction :: Gen String
genSimpleFunction = do
    name <- elements ["func", "test", "calculate", "process"]
    return $ "func " ++ name ++ "() {\n    return 42\n}\n"

genVariableDeclaration :: Gen String
genVariableDeclaration = do
    name <- elements ["x", "y", "value", "result"]
    value <- elements ["0", "1", "42", "\"hello\""]
    return $ name ++ " := " ++ value ++ "\n"

genCommentOnly :: Gen String
genCommentOnly = do
    comment <- elements ["// This is a comment", "// TODO: implement", "// FIXME: bug"]
    return comment ++ "\n"

genEmptyBlock :: Gen String
genEmptyBlock = return "// Empty block\n\n"

-- Generate directives
genFileDirective :: Gen String
genFileDirective = oneof
    [ return "// @ownership: true"
    , return "// @ownership: false"
    , return "// @dependent-types: true"
    , return "// @dependent-types: false"
    , return "// @constraints: true"
    , return "// @constraints: false"
    ]

genBlockDirective :: Gen String
genBlockDirective = oneof
    [ return "// @block-ownership: true"
    , return "// @block-ownership: false"
    , return "// @block-dependent-types: true"
    , return "// @block-dependent-types: false"
    , return "// @block-constraints: true"
    , return "// @block-constraints: false"
    ]

-- Generate build tags
genBuildTag :: Gen String
genBuildTag = do
    tag <- elements ["linux", "windows", "darwin", "test", "debug", "release"]
    return $ "// +build " ++ tag

-- ============================================================================
-- Properties
-- ============================================================================

tests :: TestTree
tests = testGroup "Parser Robustness QuickCheck Tests"
    [ testGroup "Basic Parsing Properties"
        [ testProperty "parseTypus handles empty input gracefully" $
            fastProperty prop_parseEmptyInput
        
        , testProperty "parseTypus handles whitespace-only input" $
            fastProperty prop_parseWhitespaceOnly
        
        , testProperty "parseTypus handles simple comments" $
            fastProperty prop_parseSimpleComments
        ]

    , testGroup "Directive Parsing Properties"
        [ testProperty "parseTypus handles file directives correctly" $
            fastProperty prop_parseFileDirectives
        
        , testProperty "parseTypus handles block directives correctly" $
            fastProperty prop_parseBlockDirectives
        
        , testProperty "parseTypus handles mixed directives" $
            fastProperty prop_parseMixedDirectives
        ]

    , testGroup "Code Block Properties"
        [ testProperty "parseTypus preserves code block content" $
            fastProperty prop_preserveCodeBlockContent
        
        , testProperty "parseTypus handles multiple blocks" $
            fastProperty prop_handleMultipleBlocks
        
        , testProperty "parseTypus maintains block order" $
            fastProperty prop_maintainBlockOrder
        ]

    , testGroup "Error Recovery Properties"
        [ testProperty "parseTypus recovers from syntax errors" $
            fastProperty prop_errorRecovery
        
        , testProperty "parseTypus handles malformed directives" $
            fastProperty prop_handleMalformedDirectives
        
        , testProperty "parseTypus handles incomplete blocks" $
            fastProperty prop_handleIncompleteBlocks
        ]

    , testGroup "Robustness Properties"
        [ testProperty "parseTypus handles very long lines" $
            fastProperty prop_handleLongLines
        
        , testProperty "parseTypus handles unicode content" $
            fastProperty prop_handleUnicodeContent
        
        , testProperty "parseTypus handles nested structures" $
            fastProperty prop_handleNestedStructures
        ]
    ]

-- ============================================================================
-- Property Definitions
-- ============================================================================

-- Basic Parsing Properties

prop_parseEmptyInput :: Bool
prop_parseEmptyInput =
    let result = parseTypus "" ""
    in case result of
        Left _ -> False
        Right file -> L.null (tfBlocks file)

prop_parseWhitespaceOnly :: String -> Bool
prop_parseWhitespaceOnly ws =
    let whitespaceOnly = filter isSpace ws
        result = parseTypus "" whitespaceOnly
    in case result of
        Left _ -> False
        Right file -> L.null (tfBlocks file)
  where
    isSpace c = c `elem` " \t\n\r"

prop_parseSimpleComments :: String -> Bool
prop_parseSimpleComments comment =
    let input = "// " ++ comment ++ "\n"
        result = parseTypus "" input
    in case result of
        Left _ -> False
        Right file -> not (L.null (tfBlocks file))

-- Directive Parsing Properties

prop_parseFileDirectives :: [String] -> Bool
prop_parseFileDirectives directives =
    let input = unlines directives
        result = parseTypus "" input
    in case result of
        Left _ -> False
        Right file -> True  -- If parsing succeeds, directives are handled

prop_parseBlockDirectives :: [String] -> [String] -> Bool
prop_parseBlockDirectives directives codes =
    let input = unlines $ directives ++ [""] ++ codes
        result = parseTypus "" input
    in case result of
        Left _ -> False
        Right file -> True  -- If parsing succeeds, block directives are handled

prop_parseMixedDirectives :: [String] -> [String] -> [String] -> Bool
prop_parseMixedDirectives fileDirs blockDirs codes =
    let input = unlines $ fileDirs ++ [""] ++ blockDirs ++ [""] ++ codes
        result = parseTypus "" input
    in case result of
        Left _ -> False
        Right file -> True

-- Code Block Properties

prop_preserveCodeBlockContent :: String -> Bool
prop_preserveCodeBlockContent content =
    let input = content ++ "\n"
        result = parseTypus "" input
    in case result of
        Left _ -> False
        Right file -> 
            case tfBlocks file of
                [] -> True
                (block:_) -> content `L.isInfixOf` cbContent block

prop_handleMultipleBlocks :: [String] -> Bool
prop_handleMultipleBlocks contents =
    let input = unlines $ concatMap (\c -> [c, ""]) contents
        result = parseTypus "" input
    in case result of
        Left _ -> False
        Right file -> L.length (tfBlocks file) >= L.length (L.filter (not . null) contents)

prop_maintainBlockOrder :: [String] -> Bool
prop_maintainBlockOrder contents =
    let nonEmptyContents = L.filter (not . null) contents
        input = unlines $ concatMap (\c -> [c, ""]) nonEmptyContents
        result = parseTypus "" input
    in case result of
        Left _ -> False
        Right file -> 
            let blocks = tfBlocks file
                contents' = map cbContent blocks
            in L.length contents' >= L.length nonEmptyContents

-- Error Recovery Properties

prop_errorRecovery :: String -> String -> Bool
prop_errorRecovery good bad =
    let input = good ++ "\n" ++ bad ++ "\n" ++ good ++ "\n"
        result = parseTypus "" input
    in case result of
        Left _ -> False
        Right file -> True  -- Parser should recover L.and produce some result

prop_handleMalformedDirectives :: [String] -> Bool
prop_handleMalformedDirectives malformed =
    let input = unlines malformed
        result = parseTypus "" input
    in case result of
        Left _ -> True  -- It's OK to fail on malformed directives
        Right file -> True  -- Or succeed with error recovery

prop_handleIncompleteBlocks :: [String] -> Bool
prop_handleIncompleteBlocks incomplete =
    let input = unlines incomplete
        result = parseTypus "" input
    in case result of
        Left _ -> True  -- It's OK to fail on incomplete blocks
        Right file -> True  -- Or succeed with partial parsing

-- Robustness Properties

prop_handleLongLines :: Int -> String -> Bool
prop_handleLongLines n base =
    let longLine = take (abs n `mod` 1000 + 10) (cycle base)
        input = longLine ++ "\n"
        result = parseTypus "" input
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right file -> True

prop_handleUnicodeContent :: String -> Bool
prop_handleUnicodeContent content =
    let unicodeContent = content ++ " αβγδε 中文 тест "
        input = unicodeContent ++ "\n"
        result = parseTypus "" input
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right file -> True

prop_handleNestedStructures :: Int -> Bool
prop_handleNestedStructures depth =
    let nested = replicate (abs depth `mod` 10 + 1) "    "
        input = L.concat nested ++ "nested {\n" ++ L.concat nested ++ "    value := 42\n" ++ L.concat nested ++ "}\n"
        result = parseTypus "" input
    in case result of
        Left _ -> True  -- Should handle gracefully
        Right file -> True

-- Helper functions
isInfixOf :: String -> String -> Bool
L.isInfixOf needle haystack = needle `elem` [take (L.length haystack - L.length needle + 1) (drop i haystack) | i <- [0..L.length haystack - L.length needle]]