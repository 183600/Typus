module Test.Unit.EnhancedCompilerTestSpec (tests) where

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

import Compiler
  ( compile
  , CompilerError(..)
  , CompilationPhase(..)
  , renderCompilationError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , buildTypeEnvFromPairs
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  , checkDependentTypes
  , checkOwnership
  , generateGoCode
  )
import Parser (parseTypus, TypusFile(..))
import SourceLocation (SourceSpan(..))

tests :: TestTree
tests =
  testGroup "Enhanced Compiler Tests"
    [ testCase "compiles simple valid program" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"Hello, World!\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let result = compile typusFile
            case result of
              Left errs -> assertFailure $ "compile failed: " <> unlines (map renderCompilationError errs)
              Right _ -> return ()

    , testCase "detects missing package declaration" $ do
        let source = unlines
              [ "func main() {"
              , "    println(\"no package\")"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let result = compile typusFile
            case result of
              Left errs -> 
                assertBool "should detect missing package" 
                         (L.any (\e -> "package" `L.isInfixOf` renderCompilationError e) errs)
              Right _ -> assertFailure "expected compilation to fail"

    , testCase "handles type mismatch errors" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = \"string\""  -- Type mismatch
              , "    println(x)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let result = compile typusFile
            case result of
              Left errs -> 
                assertBool "should detect type mismatch" 
                         (L.any (\e -> "type" `L.isInfixOf` renderCompilationError e) errs)
              Right _ -> assertFailure "expected compilation to fail with type error"

    , testCase "extracts function declarations correctly" $ do
        let source = unlines
              [ "package main"
              , "func add(a int, b int) int {"
              , "    return a + b"
              , "}"
              , "func multiply(x, y int) int {"
              , "    return x * y"
              , "}"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let declarations = extractDeclarations typusFile
            assertBool "should extract function declarations" 
                     (L.length declarations >= 2)
            assertBool "should find add function" 
                     (L.any ("add" `L.isInfixOf`) declarations)
            assertBool "should find multiply function" 
                     (L.any ("multiply" `L.isInfixOf`) declarations)

    , testCase "extracts function calls correctly" $ do
        let source = unlines
              [ "package main"
              , "func helper() {"
              , "    println(\"helper\")"
              , "}"
              , "func main() {"
              , "    helper()"
              , "    println(\"main\")"
              , "    unknown()"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let functionCalls = extractFunctionCalls typusFile
            assertBool "should extract function calls" 
                     (L.length functionCalls >= 3)
            assertBool "should find helper call" 
                     (L.any ("helper" `L.isInfixOf`) functionCalls)
            assertBool "should find println calls" 
                     (L.any ("println" `L.isInfixOf`) functionCalls)

    , testCase "builds type environment from pairs" $ do
        let typePairs = 
              [ ("x", "int")
              , ("y", "string")
              , ("z", "float64")
              ]
        let typeEnv = buildTypeEnvFromPairs typePairs
        assertBool "type environment should contain x" 
                 ("x" `L.isInfixOf` typeEnv)
        assertBool "type environment should contain y" 
                 ("y" `L.isInfixOf` typeEnv)
        assertBool "type environment should contain z" 
                 ("z" `L.isInfixOf` typeEnv)

    , testCase "identifies method declarations" $ do
        let source = unlines
              [ "package main"
              , "type Counter struct {"
              , "    value int"
              , "}"
              , "func (c *Counter) Increment() {"
              , "    c.value++"
              , "}"
              , "func (c Counter) Value() int {"
              , "    return c.value"
              , "}"
              , "func standalone() {}"
              , "func main() {}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let declarations = extractDeclarations typusFile
            let methods = filter isMethodDeclaration declarations
            assertBool "should identify method declarations" 
                     (L.length methods == 2)
            assertBool "should find Increment method" 
                     (L.any ("Increment" `L.isInfixOf`) methods)
            assertBool "should find Value method" 
                     (L.any ("Value" `L.isInfixOf`) methods)

    , testCase "detects malformed syntax" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    if x > 0 {"  -- Missing closing brace
              , "        println(x)"
              , "    }"
              , "}"
              ]
        case parseTypus source of
          Left _ -> return ()  -- Parsing should fail
          Right typusFile -> do
            let hasMalformed = hasMalformedSyntax typusFile
            assertBool "should detect malformed syntax" hasMalformed

    , testCase "formats compiler errors properly" $ do
        let errors = 
              [ CompilerError ParseError (SourceSpan 1 1 1 10) "syntax error"
              , CompilerError TypeError (SourceSpan 2 5 2 15) "type mismatch"
              , CompilerError DependencyError (SourceSpan 3 1 3 20) "missing import"
              ]
        let formatted = formatCompilerErrors errors
        assertBool "should include line numbers" 
                 (L.any ("1:" `L.isInfixOf`) formatted)
        assertBool "should include error types" 
                 (L.any ("syntax error" `L.isInfixOf`) formatted)
        assertBool "should include type errors" 
                 (L.any ("type mismatch" `L.isInfixOf`) formatted)

    , testCase "generates detailed error report" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = \"hello\""  -- Type error
              , "    undefined_function()"   -- Undefined function
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let result = compile typusFile
            case result of
              Left errs -> do
                let report = generateDetailedReport errs
                assertBool "report should contain error summary" 
                         (L.any ("Error" `L.isInfixOf`) report)
                assertBool "report should contain suggestions" 
                         (L.length report > 50)  -- Should be a detailed report
              Right _ -> assertFailure "expected compilation to fail"

    , testCase "analyzes errors L.and provides statistics" $ do
        let errors = 
              [ CompilerError ParseError (SourceSpan 1 1 1 10) "syntax error"
              , CompilerError TypeError (SourceSpan 2 5 2 15) "type mismatch"
              , CompilerError TypeError (SourceSpan 3 1 3 20) "invalid type"
              , CompilerError DependencyError (SourceSpan 4 1 4 20) "missing import"
              ]
        let analysis = analyzeErrors errors
        assertBool "should count type errors" 
                 (L.any ("type" `L.isInfixOf`) analysis)
        assertBool "should count parse errors" 
                 (L.any ("parse" `L.isInfixOf`) analysis)
        assertBool "should count dependency errors" 
                 (L.any ("dependency" `L.isInfixOf`) analysis)

    , testCase "checks for type errors" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    var x int = 42"
              , "    var y string = \"hello\""
              , "    x = y  -- Type error"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let result = compile typusFile
            case result of
              Left errs -> do
                let hasTypeErr = hasTypeErrors errs
                assertBool "should detect type errors" hasTypeErr
              Right _ -> assertFailure "expected compilation to fail"

    , testCase "diagnoses type checking issues" $ do
        let source = unlines
              [ "package main"
              , "func add(a int, b int) int {"
              , "    return a + b"
              , "}"
              , "func main() {"
              , "    result := add(\"hello\", \"world\")"  -- Wrong argument types
              , "    println(result)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let result = compile typusFile
            case result of
              Left errs -> do
                let diagnostics = diagnoseTypeErrors errs
                assertBool "should provide type diagnostics" 
                         (not $ null diagnostics)
                assertBool "diagnostics should mention function arguments" 
                         (L.any (\d -> "argument" `L.isInfixOf` show d) diagnostics)
              Right _ -> assertFailure "expected compilation to fail"

    , testCase "generates Go code for simple program" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    x := 42"
              , "    println(x)"
              , "}"
              ]
        case parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            let result = compile typusFile
            case result of
              Left errs -> assertFailure $ "compile failed: " <> unlines (map renderCompilationError errs)
              Right goCode -> do
                assertBool "generated Go code should contain package main" 
                         ("package main" `L.isInfixOf` goCode)
                assertBool "generated Go code should contain main function" 
                         ("func main" `L.isInfixOf` goCode)
                assertBool "generated Go code should contain variable declaration" 
                         ("x :=" `L.isInfixOf` goCode)
    ]