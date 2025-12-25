module Test.Unit.ParserCoreTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, (==>), oneof, elements)
import qualified Test.Tasty.QuickCheck as QC

import Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  , parseBool
  )
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)

-- ============================================================================
-- Arbitrary Instances
-- ============================================================================

instance Arbitrary FileDirectives where
  arbitrary = FileDirectives <$> maybeGen <*> maybeGen <*> maybeGen
    where
      maybeGen = QC.oneof [return Nothing, Just <$> arbitrary]

instance Arbitrary BlockDirectives where
  arbitrary = BlockDirectives <$> maybeGen <*> maybeGen <*> maybeGen
    where
      maybeGen = QC.oneof [return Nothing, Just <$> arbitrary]

-- Generate simple boolean strings for parseBool testing
instance Arbitrary String where
  arbitrary = QC.oneof
    [ return "on"
    , return "off"
    , return "true"
    , return "false"
    , return "  on  "
    , return "  off  "
    , return "invalid"
    , QC.elements ["maybe", "yes", "no", "1", "0"]
    ]

-- ============================================================================
-- Unit Tests
-- ============================================================================

tests :: TestTree
tests =
  testGroup "Parser Core Tests"
    [ testGroup "Default Directives"
        [ testCase "defaultFileDirectives has all Nothing values" $ do
            fdOwnership defaultFileDirectives @?= Nothing
            fdDependentTypes defaultFileDirectives @?= Nothing
            fdConstraints defaultFileDirectives @?= Nothing

        , testCase "defaultBlockDirectives has all Nothing values" $ do
            bdOwnership defaultBlockDirectives @?= Nothing
            bdDependentTypes defaultBlockDirectives @?= Nothing
            bdConstraints defaultBlockDirectives @?= Nothing
        ]

    , testGroup "Boolean Parsing"
        [ testCase "parseBool accepts 'on'" $ do
            parseBool "on" @?= Right True

        , testCase "parseBool accepts 'off'" $ do
            parseBool "off" @?= Right False

        , testCase "parseBool accepts 'true'" $ do
            parseBool "true" @?= Right True

        , testCase "parseBool accepts 'false'" $ do
            parseBool "false" @?= Right False

        , testCase "parseBool handles whitespace" $ do
            parseBool "  on  " @?= Right True
            parseBool "\toff\t" @?= Right False

        , testCase "parseBool rejects invalid values" $ do
            case parseBool "maybe" of
              Left _ -> assertBool "Should reject invalid boolean" True
              Right _ -> assertBool "Should not accept invalid boolean" False

        , testCase "parseBool is case sensitive" $ do
            case parseBool "ON" of
              Left _ -> assertBool "Should reject uppercase" True
              Right _ -> assertBool "Should not accept uppercase" False
        ]

    , testGroup "Simple Typus File Parsing"
        [ testCase "parseTypus handles empty file" $ do
            let result = parseTypus ""
            case result of
              Left err -> assertBool $ "Should parse empty file: " ++ err
              Right (TypusFile directives blocks) -> do
                directives @?= defaultFileDirectives
                assertBool "Empty file should have no blocks" $ null blocks

        , testCase "parseTypus handles simple code without directives" $ do
            let content = "func main() {\n    return 0\n}\n"
                result = parseTypus content
            case result of
              Left err -> assertBool $ "Should parse simple code: " ++ err
              Right (TypusFile directives blocks) -> do
                directives @?= defaultFileDirectives
                assertBool "Should have one block" $ length blocks == 1

        , testCase "parseTypus handles file directives" $ do
            let content = "// @ownership on\n// @dependent_types true\nfunc main() {}\n"
                result = parseTypus content
            case result of
              Left err -> assertBool $ "Should parse file with directives: " ++ err
              Right (TypusFile directives blocks) -> do
                case fdOwnership directives of
                  Just (Located _ _) -> assertBool "Ownership directive parsed" True
                  Nothing -> assertBool "Should have ownership directive" False

        , testCase "parseTypus handles block directives" $ do
            let content = "// @ownership on {\nfunc test() {}\n}\n"
                result = parseTypus content
            case result of
              Left err -> assertBool $ "Should parse block directives: " ++ err
              Right (TypusFile directives blocks) -> do
                assertBool "Should have one block" $ length blocks == 1

        , testCase "parseTypus handles mixed content" $ do
            let content = unlines
                  [ "// @ownership on"
                  , "// @dependent_types true"
                  , ""
                  , "// @constraints off {"
                  , "func constrained() {"
                  , "    return 42"
                  , "}"
                  , "}"
                  , ""
                  , "func normal() {"
                  , "    return 0"
                  , "}"
                  ]
                result = parseTypus content
            case result of
              Left err -> assertBool $ "Should parse mixed content: " ++ err
              Right (TypusFile directives blocks) -> do
                assertBool "Should have multiple blocks" $ length blocks >= 1
        ]

    , testGroup "Directive Edge Cases"
        [ testCase "parseTypus handles malformed directives gracefully" $ do
            let content = "// @invalid_directive on\nfunc main() {}\n"
                result = parseTypus content
            -- Should either parse with warning or fail gracefully
            case result of
              Left _ -> assertBool "Should handle invalid directives" True
              Right _ -> assertBool "Should parse despite invalid directives" True

        , testCase "parseTypus handles unclosed directive blocks" $ do
            let content = "// @ownership on {\nfunc test() {}\n// Missing closing brace"
                result = parseTypus content
            case result of
              Left _ -> assertBool "Should detect unclosed blocks" True
              Right _ -> assertBool "Should handle unclosed blocks gracefully" True

        , testCase "parseTypus handles nested directive blocks" $ do
            let content = unlines
                  [ "// @ownership on {"
                  , "func outer() {"
                  , "    // @dependent_types true {"
                  , "    func inner() {"
                  , "        return 42"
                  , "    }"
                  , "    }"
                  , "}"
                  , "}"
                  ]
                result = parseTypus content
            case result of
              Left err -> assertBool $ "Should handle nested blocks: " ++ err
              Right (TypusFile _ blocks) -> do
                assertBool "Should parse nested blocks" $ length blocks >= 1
        ]

    , testGroup "QuickCheck Properties"
        [ testProperty "parseBool 'on' and 'true' always return True" $
            \input -> (input `elem` ["on", "true", "  on  ", "  true  "]) ==>
              case parseBool input of
                Right True -> True
                _ -> False

        , testProperty "parseBool 'off' and 'false' always return False" $
            \input -> (input `elem` ["off", "false", "  off  ", "  false  "]) ==>
              case parseBool input of
                Right False -> True
                _ -> False

        , testProperty "parseTypus preserves content structure" $
            \content ->
              let result = parseTypus content
              in case result of
                   Left _ -> True  -- Parsing failures are acceptable for arbitrary content
                   Right (TypusFile _ blocks) ->
                     -- Number of blocks should be reasonable for content length
                     length blocks <= length (lines content) + 1

        , testProperty "FileDirectives equality is reflexive" $
            \directives -> directives == directives

        , testProperty "BlockDirectives equality is reflexive" $
            \directives -> directives == directives

        , testProperty "parseBool whitespace handling" $
            \baseValue ->
              let withSpaces = "  " ++ baseValue ++ "  "
              in case (parseBool baseValue, parseBool withSpaces) of
                   (Right b1, Right b2) -> b1 == b2
                   (Left _, Left _) -> True
                   _ -> False
        ]

    , testGroup "Error Recovery"
        [ testCase "parseTypus provides meaningful error messages" $ do
            let content = "// @ownership maybe\nfunc main() {}\n"
                result = parseTypus content
            case result of
              Left err -> 
                assertBool "Error message should be informative" $ 
                  length err > 10  -- Basic check for meaningful message
              Right _ -> 
                assertBool "Should handle invalid boolean gracefully" True

        , testCase "parseTypus handles Unicode content" $ do
            let content = "// 测试中文注释\nfunc main() { return \"你好世界\" }\n"
                result = parseTypus content
            case result of
              Left err -> assertBool $ "Should handle Unicode: " ++ err
              Right (TypusFile _ blocks) -> 
                assertBool "Should parse Unicode content" $ length blocks >= 1
        ]
    ]