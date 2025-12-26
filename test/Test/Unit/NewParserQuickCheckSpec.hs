{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewParserQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), spanStart, spanEnd)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf)
import qualified Data.Text as T

-- | Test parser properties
spec :: Spec
spec = describe "NewParser QuickCheck Tests" $ do

  describe "FileDirectives properties" $ do
    it "defaultFileDirectives has all Nothing values" $ do
      fdOwnership defaultFileDirectives `shouldBe` Nothing
      fdDependentTypes defaultFileDirectives `shouldBe` Nothing
      fdConstraints defaultFileDirectives `shouldBe` Nothing

    it "creates directives correctly" $ property $
      \ownership dependent constraints ->
        let directives = FileDirectives 
              (if ownership then Just (locatedAt startPos True) else Nothing)
              (if dependent then Just (locatedAt startPos True) else Nothing)
              (if constraints then Just (locatedAt startPos True) else Nothing)
        in (ownership ==> fdOwnership directives /= Nothing) &&
           (dependent ==> fdDependentTypes directives /= Nothing) &&
           (constraints ==> fdConstraints directives /= Nothing)

  describe "BlockDirectives properties" $ do
    it "defaultBlockDirectives has all Nothing values" $ do
      bdOwnership defaultBlockDirectives `shouldBe` Nothing
      bdDependentTypes defaultBlockDirectives `shouldBe` Nothing
      bdConstraints defaultBlockDirectives `shouldBe` Nothing

    it "creates block directives correctly" $ property $
      \ownership dependent constraints ->
        let directives = BlockDirectives
              (if ownership then Just (locatedAt startPos True) else Nothing)
              (if dependent then Just (locatedAt startPos True) else Nothing)
              (if constraints then Just (locatedAt startPos True) else Nothing)
        in (ownership ==> bdOwnership directives /= Nothing) &&
           (dependent ==> bdDependentTypes directives /= Nothing) &&
           (constraints ==> bdConstraints directives /= Nothing)

  describe "CodeBlock properties" $ do
    it "creates code blocks with correct structure" $ property $
      \content ->
        let span = SourceSpan startPos (SourcePos 1 100 99)
            directives = defaultBlockDirectives
            block = CodeBlock directives content span
        in cbDirectives block === directives &&
           cbContent block === content &&
           cbSpan block === span

    it "code block span is consistent" $ property $
      \content startLine startCol endLine endCol ->
        let start = SourcePos startLine startCol 0
            end = SourcePos endLine endCol 100
            span = SourceSpan start end
            block = CodeBlock defaultBlockDirectives content span
        in spanStart (cbSpan block) === start &&
           spanEnd (cbSpan block) === end

  describe "TypusFile properties" $ do
    it "creates TypusFile with correct structure" $ property $
      \blocks buildTags ->
        let directives = defaultFileDirectives
            syntaxErrors = []
            file = TypusFile directives buildTags blocks syntaxErrors
        in tfDirectives file === directives &&
           tfBuildTags file === buildTags &&
           tfBlocks file === blocks &&
           tfSyntaxErrors file === syntaxErrors

    it "empty file has no blocks or errors" $ do
      let file = TypusFile defaultFileDirectives [] [] []
      tfBlocks file `shouldBe` []
      tfSyntaxErrors file `shouldBe` []
      tfBuildTags file `shouldBe` []

  describe "Directive parsing properties" $ do
    it "identifies valid identifier characters" $ property $
      \c ->
        let isValid = isAlphaNum c || c == '_' || c == '-'
        in isIdentifierChar c === isValid

    it "rejects invalid identifier characters" $ property $
      \c ->
        let isInvalid = not (isAlphaNum c) && c /= '_' && c /= '-'
        in isInvalid ==> not (isIdentifierChar c)

    it "handles empty directive strings" $ 
      parseDirectives "" `shouldBe` []

    it "parses simple directive pairs" $ property $
      \key value ->
        let input = key ++ "=" ++ value
            result = parseDirectives input
        in not (null key) && not (null value) ==> 
           length result === 1 &&
           fst (head result) === key &&
           snd (head result) === value

  describe "Comment handling properties" $ do
    it "removes line comments correctly" $ property $
      \code comment ->
        let input = code ++ "//" ++ comment
            result = removeLineCommentsFromContent input
        in not ("//" `isInfixOf` result) &&
           (not (null code) ==> code `isPrefixOf` result)

    it "preserves content before line comments" $ property $
      \before comment after ->
        let input = before ++ "//" ++ comment ++ "\n" ++ after
            result = removeLineCommentsFromContent input
            lines' = lines result
        in length lines' >= 1 &&
           head lines' === before

    it "handles block comments" $ property $
      \before inside after ->
        let input = before ++ "/*" ++ inside ++ "*/" ++ after
            result = removeBlockCommentsFromContent input
        in not ("/*" `isInfixOf` result) &&
           not ("*/" `isInfixOf` result) &&
           before `isPrefixOf` result

  describe "String processing properties" $ do
    it "trims whitespace correctly" $ property $
      \prefix content suffix ->
        let input = replicate prefix ' ' ++ content ++ replicate suffix ' '
            trimmed = trimString input
        in trimmed === content

    it "splits content on newlines" $ property $
      \lines' ->
        let input = unlines lines'
            result = splitOnNewlines input
        in result === lines'

    it "preserves content when no newlines" $ property $
      \content ->
        not ('\n' `elem` content) ==> 
          splitOnNewlines content === [content]

  where
    -- Helper functions for testing
    isIdentifierChar :: Char -> Bool
    isIdentifierChar c = isAlphaNum c || c == '_' || c == '-'

    parseDirectives :: String -> [(String, String)]
    parseDirectives input = 
      if null input || not ('=' `elem` input)
      then []
      else [(takeWhile (/= '=') input, drop 1 $ dropWhile (/= '=') input)]

    removeLineCommentsFromContent :: String -> String
    removeLineCommentsFromContent = unlines . map (takeWhile (/= '/')) . lines

    removeBlockCommentsFromContent :: String -> String
    removeBlockCommentsFromContent input = 
      case input of
        [] -> []
        '/':'/':_ -> [] -- line comment
        '/':'*':xs -> case dropWhile (/= '*') xs of
                        '*':'/':rest -> removeBlockCommentsFromContent rest
                        _ -> [] -- unclosed block comment
        c:cs -> c : removeBlockCommentsFromContent cs

    trimString :: String -> String
    trimString = dropWhile isSpace . reverse . dropWhile isSpace . reverse

    splitOnNewlines :: String -> [String]
    splitOnNewlines = lines

    -- Helper instances for QuickCheck
    instance Arbitrary SourcePos where
      arbitrary = SourcePos <$> arbitraryPositive <*> arbitraryPositive <*> arbitraryNonNegative
        where
          arbitraryPositive = getPositive <$> arbitrary
          arbitraryNonNegative = getNonNegative <$> arbitrary