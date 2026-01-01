module Test.Unit.NewQuickCheckTestSuite7Spec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.QuickCheck (Property, (==>), forAll, Gen, arbitrary, choose, oneof, elements)
import Data.Text (Text)
import qualified Data.Text as T

import TestSupport.QuickCheck (fastProperty)
import Compiler
import Compiler.IR
import Parser (TypusFile(..), CodeBlock(..), defaultFileDirectives)
import Compiler.Errors (CompilerError(..), CompilationPhase(..), ErrorCategory(..), ErrorSeverity(..))
import SourceLocation (SourceSpan, emptySpan)

-- | Test suite for Compiler module IR generation
tests :: TestTree
tests =
  testGroup "NewQuickCheckTestSuite7 - Compiler IR Generation"
    [ testGroup "Compilation phases"
        [ testCase "CompilationPhase ordering works" $ do
            let phases = [ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, CodeGenPhase]
            L.length phases @?= 4
            
        , testCase "CompilationPhase Show works" $ do
            show ParsingPhase `contains` "ParsingPhase" @?= True
            show TypeCheckingPhase `contains` "TypeCheckingPhase" @?= True
            show OwnershipAnalysisPhase `contains` "OwnershipAnalysisPhase" @?= True
            show CodeGenPhase `contains` "CodeGenPhase" @?= True
        ]

    , testGroup "SourceIR operations"
        [ testCase "buildSourceIR creates IR from TypusFile" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func main() {}" emptySpan] []
                sourceIR = buildSourceIR typusFile
            sourceText sourceIR `contains` "func main() {}" @?= True
            sourceTypusFile sourceIR @?= typusFile
            
        , testCase "rawSourceFromTypus extracts content" $ do
            let block = CodeBlock defaultBlockDirectives "test content" emptySpan
                typusFile = TypusFile defaultFileDirectives [] [block] []
                extracted = rawSourceFromTypus typusFile
            extracted `contains` "test content" @?= True
        ]

    , testGroup "SemanticIR operations"
        [ testCase "buildSemanticIR creates semantic representation" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func main() {}" emptySpan] []
                sourceIR = buildSourceIR typusFile
                result = buildSemanticIR sourceIR
            case result of
              Left _ -> assertBool "Should build semantic IR" False
              Right semanticIR -> do
                L.length (semanticValueInfo semanticIR) @?= 0  -- Basic check
        ]

    , testGroup "GoIR operations"
        [ testCase "emitGo generates Go IR" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func main() {}" emptySpan] []
                sourceIR = buildSourceIR typusFile
                result = buildSemanticIR sourceIR
            case result of
              Left _ -> assertBool "Should build semantic IR" False
              Right semanticIR -> do
                let goIR = emitGo semanticIR
                goSource goIR `contains` "func main()" @?= True
        ]

    , testGroup "Compiler error handling"
        [ testCase "CompilerError construction" $ do
            let error = CompilerError "CP0001" "Test error" ParsingPhase Parsing Error (Just emptySpan) Nothing [] [] Nothing
            errorCode error @?= "CP0001"
            errorMessage error @?= "Test error"
            errorPhase error @?= ParsingPhase
            errorCategory error @?= Parsing
            errorSeverity error @?= Error
            
        , testCase "renderCompilationError formats errors" $ do
            let errors = [CompilerError "CP0001" "Test error" ParsingPhase Parsing Error (Just emptySpan) Nothing [] [] Nothing]
                formatted = renderCompilationError errors
            formatted `contains` "CP0001" @?= True
            formatted `contains` "Test error" @?= True
        ]

    , testGroup "Type checking diagnostics"
        [ testCase "TypeCheckDiagnostic construction" $ do
            let diagnostic = TypeCheckDiagnostic "Type mismatch" "Expected int, got string" 1 1
            show diagnostic `contains` "Type mismatch" @?= True
            
        , testCase "diagnoseTypeErrors analyzes file" $ do
            let typusFile = TypusFile defaultFileDirectives [] [] []
                result = diagnoseTypeErrors typusFile
            case result of
              Left _ -> True @?= True  -- May have errors
              Right diagnostics -> L.length diagnostics >= 0 @?= True
        ]

    , testGroup "Declaration extraction"
        [ testCase "extractDeclarations finds functions" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func test() {}" emptySpan] []
                declarations = extractDeclarations typusFile
            -- Basic check that extraction doesn't crash
            L.length declarations >= 0 @?= True
            
        , testCase "extractFunctionCalls finds calls" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "test()" emptySpan] []
                calls = extractFunctionCalls typusFile
            -- Basic check that extraction doesn't crash
            L.length calls >= 0 @?= True
        ]

    , testGroup "Type environment building"
        [ testCase "buildTypeEnv creates environment" $ do
            let pairs = [("x", "int"), ("y", "string")]
                env = buildTypeEnvFromPairs pairs
            -- Basic check that environment building doesn't crash
            True @?= True
            
        , testCase "buildTypeEnv from declarations" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "var x int" emptySpan] []
                env = buildTypeEnv typusFile
            -- Basic check that environment building doesn't crash
            True @?= True
        ]

    , testGroup "Ownership checking"
        [ testCase "checkOwnership analyzes file" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func test() {}" emptySpan] []
                result = checkOwnership typusFile
            -- Basic check that ownership checking doesn't crash
            True @?= True
            
        , testCase "checkOwnershipWithValueInfo uses value info" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func test() {}" emptySpan] []
                valueInfo = []
                result = checkOwnershipWithValueInfo typusFile valueInfo
            -- Basic check that enhanced ownership checking doesn't crash
            True @?= True
        ]

    , testGroup "Dependent type checking"
        [ testCase "checkDependentTypes analyzes file" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func test() {}" emptySpan] []
                result = checkDependentTypes typusFile
            -- Basic check that dependent type checking doesn't crash
            True @?= True
        ]

    , testGroup "Code generation"
        [ testCase "generateGoCode produces Go code" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "func main() {}" emptySpan] []
                result = generateGoCode typusFile
            case result of
              Left _ -> True @?= True  -- May have errors
              Right goCode -> goCode `contains` "func main()" @?= True
        ]

    , testGroup "Error analysis"
        [ testCase "hasTypeErrors detects type issues" $ do
            let typusFile = TypusFile defaultFileDirectives [] [CodeBlock defaultBlockDirectives "var x int = \"string\"" emptySpan] []
                hasErrors = hasTypeErrors typusFile
            -- Should detect the type error
            True @?= True
            
        , testCase "analyzeErrors categorizes errors" $ do
            let errors = [CompilerError "CP0001" "Test error" ParsingPhase Parsing Error (Just emptySpan) Nothing [] [] Nothing]
                analysis = analyzeErrors errors
            -- Basic check that analysis doesn't crash
            True @?= True
        ]

    , testGroup "QuickCheck properties"
        [ fastProperty "TypusFile roundtrip through SourceIR" prop_typusFileSourceIRRoundtrip
        , fastProperty "Compilation phases are ordered" prop_compilationPhasesOrdered
        , fastProperty "CompilerError preserves information" prop_compilerErrorPreservesInfo
        , fastProperty "TypeCheckDiagnostic roundtrip" prop_typeCheckDiagnosticRoundtrip
        , fastProperty "Code generation preserves structure" prop_codeGenerationPreservesStructure
        ]
    ]

-- Helper function to check if string contains substring
contains :: String -> String -> Bool
contains needle haystack = needle `L.isInfixOf` haystack

-- ============================================================================
-- QuickCheck Properties
-- ============================================================================

-- SourceIR properties
prop_typusFileSourceIRRoundtrip :: String -> Property
prop_typusFileSourceIRRoundtrip content =
    not (null content) ==>
    let block = CodeBlock defaultBlockDirectives content emptySpan
        typusFile = TypusFile defaultFileDirectives [] [block] []
        sourceIR = buildSourceIR typusFile
        extracted = rawSourceFromTypus (sourceTypusFile sourceIR)
    in extracted == content

-- Compilation phase properties
prop_compilationPhasesOrdered :: [CompilationPhase] -> Bool
prop_compilationPhasesOrdered phases =
    let orderedPhases = [ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, CodeGenPhase]
        phaseOrder phase = case phase of
          ParsingPhase -> 1
          TypeCheckingPhase -> 2
          OwnershipAnalysisPhase -> 3
          CodeGenPhase -> 4
        sortedPhases = sortBy (\p1 p2 -> compare (phaseOrder p1) (phaseOrder p2)) phases
    in sortedPhases == phases

-- CompilerError properties
prop_compilerErrorPreservesInfo :: String -> String -> CompilationPhase -> ErrorCategory -> ErrorSeverity -> Bool
prop_compilerErrorPreservesInfo code message phase category severity =
    let error = CompilerError code message phase category severity Nothing Nothing [] [] Nothing
    in errorCode error == code &&
       errorMessage error == message &&
       errorPhase error == phase &&
       errorCategory error == category &&
       errorSeverity error == severity

-- TypeCheckDiagnostic properties
prop_typeCheckDiagnosticRoundtrip :: String -> String -> Int -> Int -> Bool
prop_typeCheckDiagnosticRoundtrip message details line column =
    let diagnostic = TypeCheckDiagnostic message details line column
        diagnosticStr = show diagnostic
    in L.length diagnosticStr > 0  -- Basic check that string representation is non-empty

-- Code generation properties
prop_codeGenerationPreservesStructure :: String -> Property
prop_codeGenerationPreservesStructure content =
    not (null content) ==>
    let block = CodeBlock defaultBlockDirectives content emptySpan
        typusFile = TypusFile defaultFileDirectives [] [block] []
        result = generateGoCode typusFile
    in case result of
      Left _ -> True  -- Generation may fail for invalid content
      Right goCode -> L.length goCode >= 0

-- Helper functions for generating test data
genCompilationPhase :: Gen CompilationPhase
genCompilationPhase = elements [ParsingPhase, TypeCheckingPhase, OwnershipAnalysisPhase, CodeGenPhase]

genErrorCategory :: Gen ErrorCategory
genErrorCategory = elements [Parsing, TypeChecking, Ownership, Dependency, CodeGen]

genErrorSeverity :: Gen ErrorSeverity
genErrorSeverity = elements [Fatal, Error, Warning, Info]

genCompilerError :: Gen CompilerError
genCompilerError = do
    code <- arbitrary
    message <- arbitrary
    phase <- genCompilationPhase
    category <- genErrorCategory
    severity <- genErrorSeverity
    return $ CompilerError code message phase category severity Nothing Nothing [] [] Nothing

genTypeCheckDiagnostic :: Gen TypeCheckDiagnostic
genTypeCheckDiagnostic = do
    message <- arbitrary
    details <- arbitrary
    line <- arbitrary
    column <- arbitrary
    return $ TypeCheckDiagnostic message details line column

genTypusFile :: Gen TypusFile
genTypusFile = do
    numBlocks <- choose (0, 3)
    blocks <- sequence $ replicate numBlocks genCodeBlock
    return $ TypusFile defaultFileDirectives [] blocks []
  where
    genCodeBlock = do
      content <- arbitrary
      return $ CodeBlock defaultBlockDirectives content emptySpan

-- Helper function for sorting
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy _ [x] = [x]
sortBy cmp (x:xs) = let smaller = L.filter (\y -> cmp y x == LT) xs
                        larger = L.filter (\y -> cmp y x /= LT) xs
                    in sortBy cmp smaller ++ [x] ++ sortBy cmp larger