module Test.Unit.IntegrationEnhancedTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify)

import Utils (trim, removeComments, normalizeIndentation, splitBy)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, spanBetween, locatedAt, advancePosByText)
import Parser (parseTypus, TypusFile(..), CodeBlock(..))

-- | Enhanced integration tests for multiple modules working together
tests :: TestTree
tests =
  testGroup "Integration Enhanced Tests"
    [ testGroup "Utils and SourceLocation integration"
        [ testCase "advancePosByText and trim interaction" $ do
            let text = "   hello world   "
            let trimmed = trim text
            let startPos' = startPos
            let endPos = advancePosByText startPos' trimmed
            posLine endPos @?= 1
            posColumn endPos @?= 12  -- "hello world" length

        , testCase "splitBy and position tracking" $ do
            let text = "part1,part2,part3"
            let parts = splitBy ',' text
            let positions = scanl (\pos part -> advancePosByText pos (part ++ ",")) startPos parts
            length positions @?= 3

        , testCase "normalizeIndentation and span calculation" $ do
            let text = "    line1\n        line2\n    line3"
            let normalized = normalizeIndentation text
            let span = spanBetween startPos (advancePosByText startPos normalized)
            isValidSpan span @?= True
            posLine (spanEnd span) @?= 3
        ]

    , testGroup "Parser and SourceLocation integration"
        [ testCase "parser creates valid spans for blocks" $ do
            let content = "some code content"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should have one block" (length blocks == 1)
                let block = head blocks
                isValidSpan (cbSpan block) @?= True

        , testCase "parser preserves location information" $ do
            let content = "//! ownership=true\nsome code"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let directives = tfDirectives typusFile
                -- Check that directives have location information
                case tfDirectives typusFile of
                  FileDirectives{..} -> do
                    case fdOwnership of
                      Just (Located pos _) -> do
                        posLine pos @?= 1
                        posColumn pos @?= 4
                      Nothing -> assertBool "Should have ownership directive" False

        , testCase "parser handles multiline content with correct spans" $ do
            let content = "line1\nline2\nline3"
            case parseTypus content of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                let block = head blocks
                let span = cbSpan block
                posLine (spanEnd span) @?= 3
        ]

    , testGroup "Utils and Parser integration"
        [ testCase "removeComments before parsing" $ do
            let contentWithComments = "//! ownership=true\n/* block comment */\ncode // line comment"
            let withoutComments = removeComments contentWithComments
            case parseTypus withoutComments of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should parse after comment removal" (length blocks >= 1)

        , testCase "normalizeIndentation before parsing" $ do
            let indentedContent = "    //! ownership=true\n        some code"
            let normalized = normalizeIndentation indentedContent
            case parseTypus normalized of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let directives = tfDirectives typusFile
                case directives of
                  FileDirectives{..} -> 
                    case fdOwnership of
                      Just (Located pos _) -> posLine pos @?= 1
                      Nothing -> return ()

        , testCase "trim content before parsing" $ do
            let paddedContent = "\n  //! ownership=true\n  some code\n  "
            let trimmed = trim paddedContent
            case parseTypus trimmed of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let directives = tfDirectives typusFile
                case directives of
                  FileDirectives{..} -> 
                    case fdOwnership of
                      Just _ -> return ()
                      Nothing -> assertBool "Should parse ownership directive" False
        ]

    , testGroup "Three-way integration tests"
        [ testCase "full pipeline: utils -> parser -> location analysis" $ do
            let rawContent = "\n    //! ownership=true\n    /* comment */\n    \n    code content\n    "
            let cleaned = normalizeIndentation (trim rawContent)
            case parseTypus cleaned of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should have one block" (length blocks == 1)
                let block = head blocks
                let span = cbSpan block
                isValidSpan span @?= True
                posLine (spanStart span) @?= 1
                posLine (spanEnd span) @?= 2

        , testCase "complex content with all preprocessing steps" $ do
            let complexContent = 
                  "   \n" ++
                  "    //! ownership=true, dependent-types=true\n" ++
                  "    // +build linux\n" ++
                  "    \n" ++
                  "    /* block directive */\n" ++
                  "    \tcode with\ttabs\n" ++
                  "    \n" ++
                  "    more code // comment\n"
            let processed = normalizeIndentation (removeComments (trim complexContent))
            case parseTypus processed of
              Left err -> assertBool ("Should parse successfully: " ++ show err) False
              Right typusFile -> do
                let directives = tfDirectives typusFile
                let buildTags = tfBuildTags typusFile
                let blocks = tfBlocks typusFile
                assertBool "Should have build tags" (not (null buildTags))
                assertBool "Should have blocks" (not (null blocks))

        , testCase "error handling across modules" $ do
            let problematicContent = "//! invalid=syntax\n/* unclosed comment\ncode"
            let processed = removeComments problematicContent
            case parseTypus processed of
              Left err -> assertBool ("Should handle gracefully: " ++ show err) True
              Right typusFile -> do
                -- Should still parse what it can
                let blocks = tfBlocks typusFile
                assertBool "Should have some blocks" (length blocks >= 1)
        ]

    , testGroup "Performance and edge cases"
        [ testCase "large content handling" $ do
            let largeContent = unlines (replicate 1000 "//! ownership=true\ncode line")
            case parseTypus largeContent of
              Left err -> assertBool ("Should handle large content: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should handle many lines" (length blocks >= 1000)

        , testCase "empty and minimal content" $ do
            let emptyContent = ""
            let whitespaceContent = "   \n\t  \n   "
            case parseTypus emptyContent of
              Left err -> assertBool ("Should handle empty: " ++ show err) False
              Right _ -> return ()
            case parseTypus whitespaceContent of
              Left err -> assertBool ("Should handle whitespace: " ++ show err) False
              Right _ -> return ()

        , testCase "unicode content handling" $ do
            let unicodeContent = "//! ownership=true\n// Unicode: héllo wörld\n代码内容"
            case parseTypus unicodeContent of
              Left err -> assertBool ("Should handle unicode: " ++ show err) False
              Right typusFile -> do
                let blocks = tfBlocks typusFile
                assertBool "Should parse unicode content" (length blocks >= 1)
        ]
    ]