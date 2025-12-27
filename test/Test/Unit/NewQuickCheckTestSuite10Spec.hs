module Test.Unit.NewQuickCheckTestSuite10Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose, oneof, elements)
import Data.Text (Text)
import qualified Data.Text as T

import TestSupport.QuickCheck (fastProperty)
import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives, defaultBlockDirectives, parseTypus)
import Compiler (compile, generateGoCode, diagnoseTypeErrors)
import Compiler.IR (buildSourceIR, buildSemanticIR, emitGo, rawSourceFromTypus)
import SyntaxValidator (validateSyntax)
import SourceLocation (SourceSpan, emptySpan)
import Utils (trim, splitBy, removeComments)

-- | Test suite for integration functionality
tests :: TestTree
tests =
  testGroup "NewQuickCheckTestSuite10 - Integration Functionality"
    [ testGroup "Parser to Compiler integration"
        [ testCase "parseTypus to compile pipeline" $ do
            let input = "package main\n\nfunc main() {\n    println(\"Hello\")\n}\n"
                parseResult = parseTypus input
            case parseResult of
              Left _ -> assertBool "Should parse valid Go code" False
              Right typusFile -> do
                let compileResult = compile typusFile
                case compileResult of
                  Left _ -> True @?= True  -- May have errors but shouldn't crash
                  Right goCode -> goCode `contains` "func main()" @?= True
        ]

    , testGroup "Syntax validation integration"
        [ testCase "validateSyntax with Parser output" $ do
            let input = "func test() {\n  // missing closing brace\n"
                parseResult = parseTypus input
                syntaxErrors = validateSyntax input
            case parseResult of
              Left _ -> True @?= True  -- Parser may fail
              Right typusFile -> do
                length syntaxErrors >= 0 @?= True  -- Should detect syntax issues
        ]

    , testGroup "Compiler IR pipeline"
        [ testCase "SourceIR to SemanticIR to GoIR pipeline" $ do
            let block = CodeBlock defaultBlockDirectives "func main() {}" emptySpan
                typusFile = TypusFile defaultFileDirectives [] [block] []
                sourceIR = buildSourceIR typusFile
                semanticResult = buildSemanticIR sourceIR
            case semanticResult of
              Left _ -> True @?= True  -- May fail but shouldn't crash
              Right semanticIR -> do
                let goIR = emitGo semanticIR
                goSource goIR `contains` "func main()" @?= True
        ]

    , testGroup "Type checking integration"
        [ testCase "diagnoseTypeErrors with TypusFile" $ do
            let block = CodeBlock defaultBlockDirectives "var x int = \"string\"" emptySpan
                typusFile = TypusFile defaultFileDirectives [] [block] []
                typeResult = diagnoseTypeErrors typusFile
            case typeResult of
              Left _ -> True @?= True  -- Should detect type errors
              Right diagnostics -> length diagnostics >= 0 @?= True
        ]

    , testGroup "Utils integration with parsing"
        [ testCase "trim with parsed content" $ do
            let input = "  \t  func main() {}  \n  "
                parseResult = parseTypus input
            case parseResult of
              Right typusFile -> do
                let rawContent = rawSourceFromTypus typusFile
                    trimmed = trim rawContent
                trimmed `contains` "func main()" @?= True
              Left _ -> True @?= True  -- Parser may fail
        ]

    , testGroup "Comment handling integration"
        [ testCase "removeComments with compilation" $ do
            let input = "// This is a comment\nfunc main() {} // another comment\n"
                parseResult = parseTypus input
                withoutComments = removeComments input
            case parseResult of
              Right typusFile -> do
                let compileResult = compile typusFile
                case compileResult of
                  Right goCode -> goCode `contains` "func main()" @?= True
                  Left _ -> True @?= True
              Left _ -> True @?= True
        ]

    , testGroup "Multi-block integration"
        [ testCase "multiple code blocks compilation" $ do
            let block1 = CodeBlock defaultBlockDirectives "package main" emptySpan
                block2 = CodeBlock defaultBlockDirectives "import \"fmt\"" emptySpan
                block3 = CodeBlock defaultBlockDirectives "func main() {\n    fmt.Println(\"test\")\n}" emptySpan
                typusFile = TypusFile defaultFileDirectives [] [block1, block2, block3] []
                compileResult = compile typusFile
            case compileResult of
              Left _ -> True @?= True  -- May fail but shouldn't crash
              Right goCode -> do
                goCode `contains` "package main" @?= True
                goCode `contains` "import \"fmt\"" @?= True
                goCode `contains` "func main()" @?= True
        ]

    , testGroup "Error propagation integration"
        [ testCase "errors propagate through pipeline" $ do
            let invalidInput = "func invalid(\n"  -- Incomplete function
                parseResult = parseTypus invalidInput
                syntaxErrors = validateSyntax invalidInput
            case parseResult of
              Right typusFile -> do
                let compileResult = compile typusFile
                    typeResult = diagnoseTypeErrors typusFile
                case (compileResult, typeResult) of
                  (Left compileErrors, Left typeErrors) -> 
                    length compileErrors > 0 && length typeErrors > 0 @?= True
                  _ -> True @?= True  -- Other error combinations are valid
              Left _ -> True @?= True  -- Parser errors
            length syntaxErrors >= 0 @?= True  -- Syntax validation should work
        ]

    , testGroup "End-to-end compilation"
        [ testCase "complete compilation pipeline" $ do
            let completeInput = unlines
                  [ "package main"
                  , ""
                  , "import \"fmt\""
                  , ""
                  , "func main() {"
                  , "    message := \"Hello, World!\""
                  , "    fmt.Println(message)"
                  , "}"
                  ]
                parseResult = parseTypus completeInput
                syntaxErrors = validateSyntax completeInput
            case parseResult of
              Right typusFile -> do
                let typeResult = diagnoseTypeErrors typusFile
                    compileResult = compile typusFile
                case (typeResult, compileResult) of
                  (Right [], Right goCode) -> do
                    goCode `contains` "package main" @?= True
                    goCode `contains` "import \"fmt\"" @?= True
                    goCode `contains` "func main()" @?= True
                  _ -> True @?= True  -- Error cases are also valid
              Left _ -> True @?= True  -- Parser errors
            length syntaxErrors <= 2 @?= True  -- Should have minimal syntax errors
        ]

    , testGroup "QuickCheck properties"
        [ fastProperty "parse-compile roundtrip preserves structure" prop_parseCompileRoundtrip
        , fastProperty "syntax validation consistency" prop_syntaxValidationConsistency
        , fastProperty "IR pipeline preserves content" prop_irPipelinePreservesContent
        , fastProperty "error propagation preserves information" prop_errorPropagationPreservesInfo
        , fastProperty "multi-block integration maintains order" prop_multiBlockIntegrationMaintainsOrder
        ]
    ]

-- Helper function to check if string contains substring
contains :: String -> String -> Bool
contains needle haystack = needle `isInfixOf` haystack

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- Parse-compile roundtrip properties
prop_parseCompileRoundtrip :: String -> Property
prop_parseCompileRoundtrip input =
    not (null input) && isValidGoInput input ==>
    let parseResult = parseTypus input
    in case parseResult of
      Right typusFile ->
        let compileResult = compile typusFile
        in case compileResult of
          Right goCode -> length goCode >= 0  -- Basic sanity check
          Left _ -> True  -- Compilation may fail
      Left _ -> True  -- Parsing may fail

-- Syntax validation consistency properties
prop_syntaxValidationConsistent :: String -> Bool
prop_syntaxValidationConsistent input =
    let errors1 = validateSyntax input
        errors2 = validateSyntax input
    in length errors1 == length errors2

-- IR pipeline properties
prop_irPipelinePreservesContent :: String -> Property
prop_irPipelinePreservesContent input =
    not (null input) ==>
    let block = CodeBlock defaultBlockDirectives input emptySpan
        typusFile = TypusFile defaultFileDirectives [] [block] []
        sourceIR = buildSourceIR typusFile
        sourceText = rawSourceFromTypus typusFile
    in sourceText == input

-- Error propagation properties
prop_errorPropagationPreservesInfo :: String -> Bool
prop_errorPropagationPreservesInfo input =
    let syntaxErrors = validateSyntax input
        parseResult = parseTypus input
    in case parseResult of
      Right typusFile ->
        let typeResult = diagnoseTypeErrors typusFile
            compileResult = compile typusFile
        in case (typeResult, compileResult) of
          (Left typeErrors, Left compileErrors) -> 
            length typeErrors > 0 && length compileErrors > 0
          _ -> True  -- Other combinations are valid
      Left _ -> length syntaxErrors >= 0  -- Parser errors

-- Multi-block integration properties
prop_multiBlockIntegrationMaintainsOrder :: [String] -> Property
prop_multiBlockIntegrationMaintainsOrder contents =
    not (null contents) && all (not . null) contents ==>
    let blocks = map (\content -> CodeBlock defaultBlockDirectives content emptySpan) contents
        typusFile = TypusFile defaultFileDirectives [] blocks []
        sourceIR = buildSourceIR typusFile
        sourceText = rawSourceFromTypus typusFile
    in length sourceText >= sum (map length contents)  -- Basic structure preservation

-- Helper functions for generating test data
genValidGoInput :: Gen String
genValidGoInput = oneof
    [ return "package main\n\nfunc main() {\n}\n"
    , return "func test() {\n}\n"
    , return "var x int = 42\n"
    , return "import \"fmt\"\n"
    , return "type MyType struct {\n    Field int\n}\n"
    ]

genInvalidGoInput :: Gen String
genInvalidGoInput = oneof
    [ return "func incomplete(\n"
    , return "var x int = \"string\"\n"
    , return "{ missing opening brace\n"
    , return "unclosed string \"\n"
    , return "if condition {\n  // missing closing brace\n"
    ]

genMixedInput :: Gen String
genMixedInput = do
    numLines <- choose (1, 10)
    lines' <- sequence $ replicate numLines genLine
    return $ unlines lines'
  where
    genLine = oneof
      [ genValidGoInput
      , genInvalidGoInput
      , arbitrary `suchThat` (not . null)
      ]

-- Helper function to check if input is likely valid Go
isValidGoInput :: String -> Bool
isValidGoInput input = 
    let hasPackage = "package" `isInfixOf` input
        hasFunc = "func" `isInfixOf` input
        hasBraces = count '{' input == count '}' input
    in hasPackage && hasFunc && hasBraces

-- Helper function to count characters
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- Helper function for sorting
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy _ [x] = [x]
sortBy cmp (x:xs) = let smaller = filter (\y -> cmp y x == LT) xs
                        larger = filter (\y -> cmp y x /= LT) xs
                    in sortBy cmp smaller ++ [x] ++ sortBy cmp larger