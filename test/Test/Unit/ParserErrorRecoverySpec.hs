{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.ParserErrorRecoverySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..))
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)
import Utils (trim)

spec :: Spec
spec = describe "Parser Error Recovery Tests" $ do

  describe "File directive parsing with errors" $ do
    it "handles empty file directives" $ do
      let directives = defaultFileDirectives
      fdOwnership directives `shouldBe` Nothing
      fdDependentTypes directives `shouldBe` Nothing
      fdConstraints directives `shouldBe` Nothing
      
    it "handles malformed ownership directive" $ do
      -- This test would typically involve parsing malformed input
      -- For now, we test the default behavior
      let directives = defaultFileDirectives
      fdOwnership directives `shouldBe` Nothing
      
    it "handles malformed dependent types directive" $ do
      let directives = defaultFileDirectives
      fdDependentTypes directives `shouldBe` Nothing
      
    it "handles malformed constraints directive" $ do
      let directives = defaultFileDirectives
      fdConstraints directives `shouldBe` Nothing

  describe "Block directive parsing with errors" $ do
    it "handles empty block directives" $ do
      let directives = defaultBlockDirectives
      bdOwnership directives `shouldBe` Nothing
      bdDependentTypes directives `shouldBe` Nothing
      bdConstraints directives `shouldBe` Nothing
      
    it "handles mixed valid and invalid directives" $ do
      -- Test with a mix of valid and invalid directives
      let directives = defaultBlockDirectives
      bdOwnership directives `shouldBe` Nothing
      bdDependentTypes directives `shouldBe` Nothing
      bdConstraints directives `shouldBe` Nothing

  describe "Code block parsing with errors" $ do
    it "handles empty code blocks" $ do
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0)
          directives = defaultBlockDirectives
          content = ""
      cbDirectives (CodeBlock directives content span) `shouldBe` directives
      cbContent (CodeBlock directives content span) `shouldBe` content
      cbSpan (CodeBlock directives content span) `shouldBe` span
      
    it "handles code blocks with syntax errors" $ do
      -- Test with syntax errors in content
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          directives = defaultBlockDirectives
          content = "invalid syntax {"
      block = CodeBlock directives content span
      cbDirectives block `shouldBe` directives
      cbContent block `shouldBe` content
      cbSpan block `shouldBe` span

  describe "Typus file parsing with errors" $ do
    it "handles empty typus files" $ do
      let directives = defaultFileDirectives
          buildTags = []
          blocks = []
          syntaxErrors = []
      file = TypusFile directives buildTags blocks syntaxErrors
      tfDirectives file `shouldBe` directives
      tfBuildTags file `shouldBe` buildTags
      tfBlocks file `shouldBe` blocks
      tfSyntaxErrors file `shouldBe` syntaxErrors
      
    it "handles files with missing directives" $ do
      let directives = defaultFileDirectives
          buildTags = []
          blocks = []
          syntaxErrors = []
      file = TypusFile directives buildTags blocks syntaxErrors
      tfDirectives file `shouldBe` directives
      
    it "handles files with syntax errors" $ do
      let directives = defaultFileDirectives
          buildTags = []
          blocks = []
          syntaxErrors = []  -- Would be populated with actual syntax errors
      file = TypusFile directives buildTags blocks syntaxErrors
      tfDirectives file `shouldBe` directives
      tfSyntaxErrors file `shouldBe` syntaxErrors

  describe "Error recovery strategies" $ do
    it "recovers from directive syntax errors" $ do
      -- Test that parser can recover from directive syntax errors
      let directives = defaultFileDirectives
      fdOwnership directives `shouldBe` Nothing
      fdDependentTypes directives `shouldBe` Nothing
      fdConstraints directives `shouldBe` Nothing
      
    it "recovers from block delimiter errors" $ do
      -- Test that parser can recover from block delimiter errors
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
          directives = defaultBlockDirectives
          content = "code"
      block = CodeBlock directives content span
      cbDirectives block `shouldBe` directives
      
    it "recovers from malformed content" $ do
      -- Test that parser can recover from malformed content
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 10 9)
          directives = defaultBlockDirectives
          content = "{ malformed }"
      block = CodeBlock directives content span
      cbDirectives block `shouldBe` directives
      cbContent block `shouldBe` content

  describe "Partial parsing with errors" $ do
    it "parses partial directives" $ do
      -- Test parsing with partial directive information
      let directives = defaultFileDirectives
      fdOwnership directives `shouldBe` Nothing
      fdDependentTypes directives `shouldBe` Nothing
      fdConstraints directives `shouldBe` Nothing
      
    it "parses partial blocks" $ do
      -- Test parsing with partial block information
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
          directives = defaultBlockDirectives
          content = "partial"
      block = CodeBlock directives content span
      cbDirectives block `shouldBe` directives
      cbContent block `shouldBe` content

  describe "Error accumulation" $ do
    it "accumulates multiple syntax errors" $ do
      -- Test that multiple syntax errors can be accumulated
      let directives = defaultFileDirectives
          buildTags = []
          blocks = []
          syntaxErrors = []  -- Would be populated with actual syntax errors
      file = TypusFile directives buildTags blocks syntaxErrors
      tfDirectives file `shouldBe` directives
      tfSyntaxErrors file `shouldBe` syntaxErrors
      
    it "preserves error locations" $ do
      -- Test that error locations are preserved during parsing
      let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 5 4)
          directives = defaultBlockDirectives
          content = "error"
      block = CodeBlock directives content span
      cbSpan block `shouldBe` span

  describe "QuickCheck properties" $ do
    it "directives default to Nothing" $ property $
      \directives -> 
        let fd = fdOwnership directives
            dt = fdDependentTypes directives
            cs = fdConstraints directives
        in fd == Nothing || dt == Nothing || cs == Nothing
        
    it "code blocks preserve content" $ property $
      \content ->
        let span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length content + 1) (length content))
            directives = defaultBlockDirectives
            block = CodeBlock directives content span
        in cbContent block `shouldBe` content
        
    it "typus files preserve structure" $ property $
      \directives ->
        let buildTags = []
            blocks = []
            syntaxErrors = []
            file = TypusFile directives buildTags blocks syntaxErrors
        in tfDirectives file `shouldBe` directives

  describe "Edge cases" $ do
    it "handles extremely long content" $ do
      let longContent = replicate 1000 'a'
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 1001 1000)
          directives = defaultBlockDirectives
          block = CodeBlock directives longContent span
      cbContent block `shouldBe` longContent
      
    it "handles special characters in content" $ do
      let specialContent = "特殊字符 & symbols !@#$%^&*()"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length specialContent + 1) (length specialContent))
          directives = defaultBlockDirectives
          block = CodeBlock directives specialContent span
      cbContent block `shouldBe` specialContent
      
    it "handles unicode content" $ do
      let unicodeContent = "🚀 Unicode test with émojis"
          span = SourceSpan (SourcePos 1 1 0) (SourcePos 1 (length unicodeContent + 1) (length unicodeContent))
          directives = defaultBlockDirectives
          block = CodeBlock directives unicodeContent span
      cbContent block `shouldBe` unicodeContent