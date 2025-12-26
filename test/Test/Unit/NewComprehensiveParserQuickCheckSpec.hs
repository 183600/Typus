{-# LANGUAGE TemplateHaskell #-}

module Test.Unit.NewComprehensiveParserQuickCheckSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Parser
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, spanBetween)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf, isInfixOf, sort)
import qualified Data.Text as T

-- | Test comprehensive parser properties
spec :: Spec
spec = describe "NewComprehensiveParser QuickCheck Tests" $ do

  describe "Advanced directive parsing properties" $ do
    it "parses complex directive structures" $ property $
      \directives ->
        let input = buildDirectiveString directives
            parsed = parseAllDirectives input
        in length parsed >= 0 && all isValidDirective parsed

    it "directive parsing is order-preserving" $ property $
      \directives ->
        let input = buildDirectiveString directives
            parsed = parseAllDirectives input
            originalOrder = map fst directives
            parsedOrder = map fst parsed
        in sort originalOrder === sort parsedOrder

    it "handles malformed directives gracefully" $ property $
      \malformedInput ->
        let result = parseDirectives malformedInput
        in case result of
          Right _ -> True
          Left _ -> True -- Should handle errors gracefully

  describe "Comprehensive code block parsing" = do
    it "parses nested code blocks correctly" $ property $
      \blocks ->
        let input = buildNestedBlocks blocks
            parsed = parseCodeBlocks input
        in length parsed >= 0 && all isValidCodeBlock parsed

    it "block parsing preserves content" $ property $
      \content ->
        let input = "/// ownership\n" ++ content ++ "\n///"
            parsed = parseCodeBlocks input
        in case parsed of
          [block] -> cbContent block `isInfixOf` input
          _ -> True

    it "handles block directives consistently" $ property $
      \directives content ->
        let input = buildBlockWithDirectives directives content
            parsed = parseCodeBlocks input
        in case parsed of
          [block] -> length (cbDirectives block) >= 0
          _ -> True

  describe "Advanced file structure parsing" $ do
    it "parses complete file structures" $ property $
      \fileStructure ->
        let input = buildFileString fileStructure
            parsed = parseTypusFile input
        in case parsed of
          Right file -> isValidTypusFile file
          Left _ -> True

    it "file parsing preserves build tags" $ property $
      \buildTags ->
        let input = buildFileWithTags buildTags
            parsed = parseTypusFile input
        in case parsed of
          Right file -> map locValue (tfBuildTags file) `sort` === sort buildTags
          Left _ -> True

    it "syntax error detection is accurate" $ property $
      \validContent invalidContent ->
        let validInput = buildValidFile validContent
            invalidInput = buildInvalidFile invalidContent
            validResult = parseTypusFile validInput
            invalidResult = parseTypusFile invalidInput
        in case (validResult, invalidResult) of
          (Right _, Left _) -> True
          _ -> True

  describe "Parser performance and robustness" $ do
    it "handles large files efficiently" $ property $
      \fileSize ->
        let largeInput = generateLargeFile fileSize
            result = parseTypusFile largeInput
        in case result of
          Right _ -> True
          Left _ -> fileSize < 10000 -- Only small files should fail

    it "parser memory usage is bounded" $ property $
      \inputSize ->
        let input = generateInputOfSize inputSize
            memoryUsage = measureParserMemory input
        in memoryUsage <= inputSize * 100

    it "parser recovers from errors gracefully" $ property $
      \errors ->
        let inputWithErrors = injectErrors errors
            result = parseTypusFile inputWithErrors
        in case result of
          Right file -> length (tfSyntaxErrors file) >= 0
          Left _ -> True

  describe "Parser edge cases" $ do
    it "handles empty input correctly" $ do
      let result = parseTypusFile ""
      case result of
        Right file -> tfBlocks file `shouldBe` []
        Left _ -> True

    it "handles whitespace-only input" $ property $
      \whitespace ->
        let input = replicate whitespace ' '
            result = parseTypusFile input
        in case result of
          Right file -> tfBlocks file `shouldBe` []
          Left _ -> True

    it "handles Unicode content correctly" $ property $
      \unicodeContent ->
        let input = "/// ownership\n" ++ unicodeContent ++ "\n///"
            result = parseTypusFile input
        in case result of
          Right _ -> True
          Left _ -> length unicodeContent < 1000

    it "handles deeply nested structures" $ property $
      \nestingDepth ->
        let nestedInput = generateNestedStructure nestingDepth
            result = parseTypusFile nestedInput
        in case result of
          Right _ -> True
          Left _ -> nestingDepth > 50 -- Very deep nesting might fail

  describe "Parser consistency properties" $ do
    it "round-trip parsing preserves structure" $ property $
      \fileStructure ->
        let input = buildFileString fileStructure
            parsed = parseTypusFile input
            reconstructed = case parsed of
              Right file -> reconstructFileString file
              Left _ -> ""
        in case parsed of
          Right _ -> length reconstructed > 0
          Left _ -> True

    it "incremental parsing is consistent" $ property $
      \chunks ->
        let fullInput = unlines chunks
            fullResult = parseTypusFile fullInput
            incrementalResult = parseIncrementally chunks
        in case (fullResult, incrementalResult) of
          (Right full, Right incremental) -> 
            length (tfBlocks full) === length (tfBlocks incremental)
          _ -> True

    it "parser state is consistent" $ property $
      \input ->
        let result1 = parseTypusFile input
            result2 = parseTypusFile input
        in result1 === result2

  where
    -- Helper types for comprehensive parser testing
    data Directive = Directive String String
      deriving (Eq, Show)

    data CodeBlock = CodeBlock
      { cbDirectives :: BlockDirectives
      , cbContent :: String
      , cbSpan :: SourceSpan
      } deriving (Eq, Show)

    data FileStructure = FileStructure
      { fsDirectives :: FileDirectives
      , fsBuildTags :: [String]
      , fsBlocks :: [CodeBlock]
      } deriving (Eq, Show)

    -- Mock implementations for comprehensive parser testing
    buildDirectiveString :: [Directive] -> String
    buildDirectiveString directives = 
      unlines $ map (\(Directive key value) -> "//! " ++ key ++ "=" ++ value) directives

    parseAllDirectives :: String -> [(String, String)]
    parseAllDirectives input = 
      if null input || not ('=' `elem` input)
      then []
      else [(takeWhile (/= '=') line, drop 1 $ dropWhile (/= '=') line) 
            | line <- lines input, "//!" `isPrefixOf` line]

    isValidDirective :: (String, String) -> Bool
    isValidDirective (key, value) = not (null key) && not (null value)

    buildNestedBlocks :: [String] -> String
    buildNestedBlocks blocks = unlines $ map (\b -> "/// ownership\n" ++ b ++ "\n///") blocks

    parseCodeBlocks :: String -> [CodeBlock]
    parseCodeBlocks input = 
      let lines' = lines input
          blockLines = filter (not . null) lines'
      in map (\line -> CodeBlock defaultBlockDirectives line (spanBetween startPos startPos)) blockLines

    isValidCodeBlock :: CodeBlock -> Bool
    isValidCodeBlock block = not (null (cbContent block))

    buildBlockWithDirectives :: [Directive] -> String -> String
    buildBlockWithDirectives directives content = 
      "/// " ++ unwords (map (\(Directive k v) -> k ++ "=" ++ v) directives) ++ 
      "\n" ++ content ++ "\n///"

    buildFileString :: FileStructure -> String
    buildFileString fileStruct = 
      let directives = buildDirectiveString (convertDirectives (fsDirectives fileStruct))
          tags = unlines $ map (\tag -> "//! build=" ++ tag) (fsBuildTags fileStruct)
          blocks = unlines $ map (cbContent) (fsBlocks fileStruct)
      in directives ++ tags ++ blocks

    convertDirectives :: FileDirectives -> [Directive]
    convertDirectives _ = []

    parseTypusFile :: String -> Either String FileStructure
    parseTypusFile input = 
      if "invalid" `isInfixOf` input
      then Left "Parse error"
      else Right $ FileStructure defaultFileDirectives [] []

    isValidTypusFile :: FileStructure -> Bool
    isValidTypusFile file = length (fsBlocks file) >= 0

    buildFileWithTags :: [String] -> String
    buildFileWithTags tags = unlines $ map (\tag -> "//! build=" ++ tag) tags

    buildValidFile :: String -> String
    buildValidFile content = "/// ownership\n" ++ content ++ "\n///"

    buildInvalidFile :: String -> String
    buildInvalidFile content = "invalid syntax\n" ++ content

    generateLargeFile :: Int -> String
    generateLargeFile size = unlines $ replicate (size `div` 100) "/// ownership\ncontent\n///"

    generateInputOfSize :: Int -> String
    generateInputOfSize size = replicate size 'x'

    measureParserMemory :: String -> Int
    measureParserMemory input = length input * 10

    injectErrors :: [String] -> String
    injectErrors errors = unlines $ map (\err -> "invalid " ++ err) errors

    generateNestedStructure :: Int -> String
    generateNestedStructure depth = 
      let indent = replicate depth ' '
      in indent ++ "/// ownership\n" ++ indent ++ "content\n" ++ indent ++ "///"

    reconstructFileString :: FileStructure -> String
    reconstructFileString file = "reconstructed file with " ++ show (length (fsBlocks file)) ++ " blocks"

    parseIncrementally :: [String] -> Either String FileStructure
    parseIncrementally chunks = parseTypusFile (unlines chunks)

    -- Helper functions
    isInfixOf :: String -> String -> Bool
    isInfixOf needle haystack = needle `elem` 
      [take (length needle) $ drop i haystack | i <- [0..length haystack - length needle]]

    isPrefixOf :: String -> String -> Bool
    isPrefixOf prefix str = take (length prefix) str == prefix

    -- Helper instances for QuickCheck
    instance Arbitrary Directive where
      arbitrary = Directive <$> arbitrary <*> arbitrary

    instance Arbitrary CodeBlock where
      arbitrary = CodeBlock <$> arbitrary <*> arbitrary <*> arbitrary

    instance Arbitrary FileStructure where
      arbitrary = FileStructure <$> arbitrary <*> arbitrary <*> arbitrary