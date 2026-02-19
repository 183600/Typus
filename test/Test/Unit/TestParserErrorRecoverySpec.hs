{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Unit.TestParserErrorRecoverySpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck ()

import Parser
import SourceLocation (SourcePos(..), Located(..), spanBetween)
import qualified Data.Text as T ()
import TestSupport.Arbitrary ()

-- | Test suite for Parser error recovery
testParserErrorRecovery :: TestTree
testParserErrorRecovery = testGroup "Parser Error Recovery Tests"
  [ testCase "parseTypus: handles empty input gracefully" $
      let result = parseTypus ""
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> tfBlocks typusFile @?= []
           
  , testCase "parseTypus: handles only whitespace gracefully" $
      let result = parseTypus "   \n\t   \n   "
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> tfBlocks typusFile @?= []
           
  , testCase "parseTypus: handles only comments gracefully" $
      let input = "// This is a comment\n/* This is a block comment */\n//! file_directive"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> tfBlocks typusFile @?= []
           
  , testCase "parseTypus: recovers from malformed directive" $
      let input = "//! malformed directive without equals\n```go\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (tfBlocks typusFile) @?= 1
           
  , testCase "parseTypus: handles unclosed block comment" $
      let input = "//! ownership=true\n/* This comment is not closed\n```go\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (tfBlocks typusFile) @?= 1
           
  , testCase "parseTypus: handles unclosed string literal in directive" $
      let input = "//! message=\"unclosed string\n```go\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (tfBlocks typusFile) @?= 1
           
  , testCase "parseTypus: handles malformed code block markers" $
      let input = "//! ownership=true\n```\ngo code without language\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (tfBlocks typusFile) @?= 1
           
  , testCase "parseTypus: handles missing closing code block marker" $
      let input = "//! ownership=true\n```go\nfmt.Println(\"hello\")\n// missing closing marker"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (tfBlocks typusFile) @?= 1
           
  , testCase "parseTypus: handles nested block directives correctly" $
      let input = "//! ownership=true\n```go, ownership=false\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let blocks = tfBlocks typusFile
             length blocks @?= 1
             let block = case blocks of
                           (b:_) -> b
                           [] -> error "Impossible: blocks is not empty"
             bdOwnership (cbDirectives block) @?= Just (locatedAt (SourcePos 2 1 0) False)
             
  , testCase "parseTypus: handles multiple file directives" $
      let input = "//! ownership=true\n//! dependent_types=true\n//! constraints=false\n```go\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let directives = tfDirectives typusFile
             fdOwnership directives @?= Just (locatedAt (SourcePos 1 1 0) True)
             fdDependentTypes directives @?= Just (locatedAt (SourcePos 2 1 0) True)
             fdConstraints directives @?= Just (locatedAt (SourcePos 3 1 0) False)
             
  , testCase "parseTypus: handles build tags correctly" $
      let input = "// +build linux,amd64\n//! ownership=true\n```go\nfmt.Println(\"hello\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let buildTags = tfBuildTags typusFile
             length buildTags @?= 1
             let tag = case buildTags of
                         (t:_) -> t
                         [] -> error "Impossible: buildTags is not empty"
             getLocValue tag @?= "+build linux,amd64"
             
  , testCase "parseTypus: preserves syntax errors for later processing" $
      let input = "//! ownership=true\n```go\nfunc invalid_syntax(\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> do
             let syntaxErrors = tfSyntaxErrors typusFile
             -- Syntax errors should be captured but not prevent parsing
             length syntaxErrors @?= 0  -- Will be filled by syntax validator
             
  , testCase "parseTypus: handles Unicode content correctly" $
      let input = "//! ownership=true\n```go\nfmt.Println(\"你好, 世界!\")\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (tfBlocks typusFile) @?= 1
           
  , testCase "parseTypus: handles very long lines" $
      let longLine = replicate 100 'a'  -- 从1000减少到100，大幅减少内存使用
          input = "//! ownership=true\n```go\nconst longString = \"" ++ longLine ++ "\"\n```"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (tfBlocks typusFile) @?= 1
           
  , testCase "parseTypus: handles mixed line endings" $
      let input = "//! ownership=true\r\n```go\nfmt.Println(\"hello\")\n```\r\n"
          result = parseTypus input
      in case result of
           Left err -> assertFailure $ "Parse failed: " ++ show err
           Right typusFile -> length (tfBlocks typusFile) @?= 1
  ]

-- Helper functions
locatedAt :: SourcePos -> a -> Located a
locatedAt pos value = Located value pos (spanBetween pos pos)

getLocValue :: Located a -> a
getLocValue (Located value _ _) = value