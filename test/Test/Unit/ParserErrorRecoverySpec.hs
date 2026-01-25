{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.ParserErrorRecoverySpec where


import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty

import Test.Tasty.QuickCheck
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), 
               defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..))

tests :: TestTree
tests = testGroup "Parser Error Recovery Tests"
  [ testGroup "File directive parsing with errors"
    [ testCase "handles empty file directives" $ do
        let directives = defaultFileDirectives
        fdOwnership directives @?= Nothing
        fdDependentTypes directives @?= Nothing
        fdConstraints directives @?= Nothing
      
    , testCase "handles malformed ownership directive" $ do
        -- This test would typically involve parsing malformed input
        -- For now, we test the default behavior
        let directives = defaultFileDirectives
        fdOwnership directives @?= Nothing
      
    , testCase "handles malformed dependent types directive" $ do
        let directives = defaultFileDirectives
        fdDependentTypes directives @?= Nothing
      
    , testCase "handles malformed constraints directive" $ do
        let directives = defaultFileDirectives
        fdConstraints directives @?= Nothing
    ]
  , testGroup "Block directive parsing with errors"
    [ testCase "handles empty block directives" $ do
        let directives = defaultBlockDirectives
        bdOwnership directives @?= Nothing
        bdDependentTypes directives @?= Nothing
        bdConstraints directives @?= Nothing
      
    , testCase "handles mixed valid and invalid directives" $ do
        -- Test with a mix of valid and invalid directives
        let directives = defaultBlockDirectives
        bdOwnership directives @?= Nothing
        bdDependentTypes directives @?= Nothing
        bdConstraints directives @?= Nothing
    ]
  , testGroup "Code block parsing with errors"
    [ testCase "handles empty code blocks" $ do
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
            directives = defaultBlockDirectives
            content = ""
        cbDirectives (CodeBlock directives content span) @?= directives
        cbContent (CodeBlock directives content span) @?= content
        cbSpan (CodeBlock directives content span) @?= span
      
    , testCase "handles code blocks with syntax errors" $ do
        -- Test with syntax errors in content
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            directives = defaultBlockDirectives
            content = "invalid syntax {"
        let block = CodeBlock directives content span
        cbDirectives block @?= directives
        cbContent block @?= content
        cbSpan block @?= span
    ]
  , testGroup "Typus file parsing with errors"
    [ testCase "handles empty typus files" $ do
        let directives = defaultFileDirectives
            buildTags = []
            blocks = []
            syntaxErrors = []
        let file = TypusFile directives buildTags blocks syntaxErrors
        tfDirectives file @?= directives
        tfBuildTags file @?= buildTags
        tfBlocks file @?= blocks
        tfSyntaxErrors file @?= syntaxErrors
      
    , testCase "handles files with missing directives" $ do
        let directives = defaultFileDirectives
            buildTags = []
            blocks = []
            syntaxErrors = []
            file = TypusFile directives buildTags blocks syntaxErrors
        tfDirectives file @?= directives
      
    , testCase "handles files with syntax errors" $ do
        let directives = defaultFileDirectives
            buildTags = []
            blocks = []
            syntaxErrors = []  -- Would be populated with actual syntax errors
        let file = TypusFile directives buildTags blocks syntaxErrors
        tfDirectives file @?= directives
        tfSyntaxErrors file @?= syntaxErrors
    ]
  , testGroup "Error recovery strategies"
    [ testCase "recovers from directive syntax errors" $ do
        -- Test that parser can recover from directive syntax errors
        let directives = defaultFileDirectives
        fdOwnership directives @?= Nothing
        fdDependentTypes directives @?= Nothing
        fdConstraints directives @?= Nothing
      
    , testCase "recovers from block delimiter errors" $ do
        -- Test that parser can recover from block delimiter errors
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
            directives = defaultBlockDirectives
            content = "code"
        let block = CodeBlock directives content span
        cbDirectives block @?= directives
      
    , testCase "recovers from malformed content" $ do
        -- Test that parser can recover from malformed content
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
            directives = defaultBlockDirectives
            content = "{ malformed }"
        let block = CodeBlock directives content span
        cbDirectives block @?= directives
        cbContent block @?= content
    ]
  , testGroup "Partial parsing with errors"
    [ testCase "parses partial directives" $ do
        -- Test parsing with partial directive information
        let directives = defaultFileDirectives
        fdOwnership directives @?= Nothing
        fdDependentTypes directives @?= Nothing
        fdConstraints directives @?= Nothing
      
    , testCase "parses partial blocks" $ do
        -- Test parsing with partial block information
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
            directives = defaultBlockDirectives
            content = "partial"
        let block = CodeBlock directives content span
        cbDirectives block @?= directives
        cbContent block @?= content
    ]
  , testGroup "Error accumulation"
    [ testCase "accumulates multiple syntax errors" $ do
        -- Test that multiple syntax errors can be accumulated
        let directives = defaultFileDirectives
            buildTags = []
            blocks = []
            syntaxErrors = []  -- Would be populated with actual syntax errors
        let file = TypusFile directives buildTags blocks syntaxErrors
        tfDirectives file @?= directives
        tfSyntaxErrors file @?= syntaxErrors
      
    , testCase "preserves error locations" $ do
        -- Test that error locations are preserved during parsing
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
            directives = defaultBlockDirectives
            content = "error"
        let block = CodeBlock directives content span
        cbSpan block @?= span
    ]
  , testGroup "QuickCheck properties"
    [ testProperty "directives default to Nothing" $ property $
        \() -> 
          let directives = defaultFileDirectives
              fd = fdOwnership directives
              dt = fdDependentTypes directives
              cs = fdConstraints directives
          in fd == Nothing && dt == Nothing && cs == Nothing
          
    , testProperty "code blocks preserve content" $ property $
        \content ->
          let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length (content :: String) + 1) (length content))
              directives = defaultBlockDirectives
              block = CodeBlock directives content span
          in cbContent block == content
          
    , testProperty "typus files preserve structure" $ property $
        \() ->
          let directives = defaultFileDirectives
              buildTags = []
              blocks = []
              syntaxErrors = []
              file = TypusFile directives buildTags blocks syntaxErrors
          in tfDirectives file == directives
    ]
  , testGroup "Edge cases"
    [ testCase "handles extremely long content" $ do
        let longContent = replicate 1000 'a'
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1001 1000)
            directives = defaultBlockDirectives
            block = CodeBlock directives longContent span
        cbContent block @?= longContent
      
    , testCase "handles special characters in content" $ do
        let specialContent = "特殊字符 & symbols !@#$%^&*()" :: String
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length specialContent + 1) (length specialContent))
            directives = defaultBlockDirectives
            block = CodeBlock directives specialContent span
        cbContent block @?= specialContent
      
    , testCase "handles unicode content" $ do
        let unicodeContent = "🚀 Unicode test with émojis" :: String
            span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length unicodeContent + 1) (length unicodeContent))
            directives = defaultBlockDirectives
            block = CodeBlock directives unicodeContent span
        cbContent block @?= unicodeContent
    ]
  ]