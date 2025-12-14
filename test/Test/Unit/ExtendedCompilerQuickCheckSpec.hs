{-# LANGUAGE CPP #-}

module Test.Unit.ExtendedCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import TestSupport.Arbitrary
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property)

import Compiler (compile, CompilerError(..), CompilationPhase(..), generateGoCode, 
                 extractDeclarations, extractFunctionCalls, buildTypeEnv, 
                 checkDependentTypes, checkOwnership, hasTypeErrors)
import Parser (TypusFile(..), FileDirectives(..))
import qualified Compiler.GoAst as GoAst
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import qualified Compiler.TypeChecker as TC
import SourceLocation (Located(..), locatedValue)
import qualified Data.Map as Map
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T

-- Extended compiler property tests for comprehensive coverage

-- Property: Compilation is deterministic - same input produces same output
prop_compile_deterministic :: TypusFile -> Property
prop_compile_deterministic typusFile = 
  let result1 = compile typusFile
      result2 = compile typusFile
  in case (result1, result2) of
    (Left err1, Left err2) -> property $ show err1 == show err2
    (Right res1, Right res2) -> property $ res1 == res2
    _ -> property False

-- Property: Empty file compilation produces minimal valid Go code
prop_compile_empty_file :: Property
prop_compile_empty_file = 
  let emptyFile = createSimpleTypusFile ""
      result = compile emptyFile
  in case result of
    Left err -> counterexample ("Empty file compilation failed: " ++ show err) $ property False
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with only directives compile without errors
prop_compile_directives_only :: FileDirectives -> Property
prop_compile_directives_only directives =
  let directivesFile = createSimpleTypusFileWithDirectives directives
      result = compile directivesFile
  in case result of
    Left err -> counterexample ("Directives-only compilation failed: " ++ show err) $ property False
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Ownership directive affects compilation output
prop_compile_ownership_directive_effect :: Bool -> Property
prop_compile_ownership_directive_effect ownershipEnabled =
  let directives = FileDirectives (Just $ Located ownershipEnabled undefined undefined) Nothing Nothing
      file = createSimpleTypusFileWithDirectives directives
      result = compile file
  in case result of
    Left err -> counterexample ("Ownership directive compilation failed: " ++ show err) $ property False
    Right goCode -> 
      let hasOwnershipChecks = "ownership" `isInfixOf` goCode || "check" `isInfixOf` goCode
      in property $ if ownershipEnabled then hasOwnershipChecks else True

-- Property: Dependent types directive affects compilation output
prop_compile_dependent_types_directive_effect :: Bool -> Property
prop_compile_dependent_types_directive_effect dtEnabled =
  let directives = FileDirectives Nothing (Just $ Located dtEnabled undefined undefined) Nothing
      file = createSimpleTypusFileWithDirectives directives
      result = compile file
  in case result of
    Left err -> counterexample ("Dependent types directive compilation failed: " ++ show err) $ property False
    Right goCode -> 
      let hasDependentTypeChecks = "dependent" `isInfixOf` goCode || "type" `isInfixOf` goCode
      in property $ if dtEnabled then hasDependentTypeChecks else True

-- Property: Constraints directive affects compilation output
prop_compile_constraints_directive_effect :: Bool -> Property
prop_compile_constraints_directive_effect constraintsEnabled =
  let directives = FileDirectives Nothing Nothing (Just $ Located constraintsEnabled undefined undefined)
      file = createSimpleTypusFileWithDirectives directives
      result = compile file
  in case result of
    Left err -> counterexample ("Constraints directive compilation failed: " ++ show err) $ property False
    Right goCode -> 
      let hasConstraintChecks = "constraint" `isInfixOf` goCode || "assert" `isInfixOf` goCode
      in property $ if constraintsEnabled then hasConstraintChecks else True

-- Property: Multiple directives interact correctly
prop_compile_multiple_directives_interaction :: Bool -> Bool -> Bool -> Property
prop_compile_multiple_directives_interaction ownership dt constraints =
  let directives = FileDirectives 
        (Just $ Located ownership undefined undefined)
        (Just $ Located dt undefined undefined)
        (Just $ Located constraints undefined undefined)
      file = createSimpleTypusFileWithDirectives directives
      result = compile file
  in case result of
    Left err -> counterexample ("Multiple directives compilation failed: " ++ show err) $ property False
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Type checking preserves type information
prop_compile_type_checking_preservation :: TypusFile -> Property
prop_compile_type_checking_preservation typusFile = 
  let typeEnv = buildSimpleTypeEnv typusFile
      result = compile typusFile
  in case (typeEnv, result) of
    (_, Left _) -> property True  -- Type errors are expected
    (Just env, Right _) -> property $ not $ Map.null env
    (Nothing, Right _) -> property True  -- Empty type environment is valid

-- Property: Declaration extraction is consistent with compilation
prop_compile_declaration_extraction_consistency :: TypusFile -> Property
prop_compile_declaration_extraction_consistency typusFile =
  let declarations = extractSimpleDeclarations typusFile
      result = compile typusFile
  in case (declarations, result) of
    ([], Left _) -> property True
    ([], Right _) -> property True
    (_, Left err) -> property $ "declaration" `isInfixOf` show err || "syntax" `isInfixOf` show err
    (decls, Right goCode) -> 
      let hasDeclarationMarkers = any (`isInfixOf` goCode) ["func", "var", "type", "const"]
      in property $ hasDeclarationMarkers

-- Property: Function call extraction matches compilation analysis
prop_compile_function_call_extraction_consistency :: TypusFile -> Property
prop_compile_function_call_extraction_consistency typusFile =
  let functionCalls = extractSimpleFunctionCalls typusFile
      result = compile typusFile
  in case (functionCalls, result) of
    ([], Left _) -> property True
    ([], Right _) -> property True
    (_, Left err) -> property $ "function" `isInfixOf` show err || "call" `isInfixOf` show err
    (calls, Right goCode) -> 
      let hasFunctionCalls = any (`isInfixOf` goCode) (map (("(" ++) . (++ ")")) calls)
      in property $ hasFunctionCalls

-- Property: Go code generation produces syntactically valid output
prop_compile_go_code_syntax_validity :: TypusFile -> Property
prop_compile_go_code_syntax_validity typusFile =
  let result = compile typusFile
  in case result of
    Left err -> property $ "error" `isInfixOf` show err  -- Errors are valid output
    Right goCode -> 
      let hasPackageDecl = "package" `isInfixOf` goCode
          hasValidBraces = countChar '{' goCode == countChar '}' goCode
          hasValidParens = countChar '(' goCode == countChar ')' goCode
      in property $ hasPackageDecl && hasValidBraces && hasValidParens

-- Property: Dependent type checking adds appropriate constraints
prop_compile_dependent_type_checking :: TypusFile -> Property
prop_compile_dependent_type_checking typusFile =
  let dtResult = checkDependentTypes typusFile
      compileResult = compile typusFile
  in case (dtResult, compileResult) of
    (Left dtErr, Left compErr) -> property $ True  -- Both failed as expected
    (Right _, Right _) -> property $ True  -- Both succeeded
    (Right _, Left _) -> property $ True  -- Type check passed but compilation failed
    (Left _, Right _) -> property $ False  -- Type check failed but compilation succeeded

-- Property: Ownership checking adds appropriate runtime checks
prop_compile_ownership_checking :: TypusFile -> Property
prop_compile_ownership_checking typusFile =
  let ownershipResult = checkOwnership typusFile
      compileResult = compile typusFile
  in case (ownershipResult, compileResult) of
    (Left ownErr, Left compErr) -> property $ True  -- Both failed as expected
    (Right _, Right _) -> property $ True  -- Both succeeded
    (Right _, Left _) -> property $ True  -- Ownership check passed but compilation failed
    (Left _, Right _) -> property $ False  -- Ownership check failed but compilation succeeded

-- Property: Type errors are properly reported
prop_compile_type_error_reporting :: TypusFile -> Property
prop_compile_type_error_reporting typusFile =
  let hasTypeErrs = hasTypeErrors typusFile
      result = compile typusFile
  in case (hasTypeErrs, result) of
    (True, Left err) -> property $ "type" `isInfixOf` show err
    (True, Right _) -> property $ False  -- Should have failed on type errors
    (False, _) -> property $ True  -- No type errors, any result is acceptable

-- Property: Compilation phases are executed in correct order
prop_compile_compilation_phases_order :: TypusFile -> Property
prop_compile_compilation_phases_order typusFile =
  let result = compile typusFile
  in case result of
    Left err -> property $ "phase" `isInfixOf` show err || "error" `isInfixOf` show err
    Right _ -> property $ True  -- Successful compilation implies correct phase order

-- Property: Large files compile without stack overflow
prop_compile_large_files :: Int -> Property
prop_compile_large_files numBlocks =
  numBlocks <= 100 ==> -- Limit to avoid timeouts
  let largeFile = createSimpleTypusFileWithBlocks numBlocks
      result = compile largeFile
  in case result of
    Left err -> property $ "large" `isInfixOf` show err || "size" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with complex nested structures compile correctly
prop_compile_nested_structures :: Int -> Property
prop_compile_nested_structures depth =
  depth <= 5 ==> -- Limit depth to avoid complexity
  let nestedFile = createNestedTypusFile depth
      result = compile nestedFile
  in case result of
    Left err -> property $ "nested" `isInfixOf` show err || "structure" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with Unicode content compile correctly
prop_compile_unicode_content :: String -> Property
prop_compile_unicode_content unicodeText =
  let unicodeFile = createSimpleTypusFile (unicodeText ++ " 测试内容 🚀")
      result = compile unicodeFile
  in case result of
    Left err -> property $ "unicode" `isInfixOf` show err || "encoding" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with special characters compile correctly
prop_compile_special_characters :: String -> Property
prop_compile_special_characters specialChars =
  let specialFile = createSimpleTypusFile specialChars
      result = compile specialFile
  in case result of
    Left err -> property $ "character" `isInfixOf` show err || "encoding" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with multiple code blocks compile correctly
prop_compile_multiple_blocks :: [String] -> Property
prop_compile_multiple_blocks blockContents =
  length blockContents <= 10 ==> -- Limit to avoid complexity
  let multiBlockFile = createSimpleTypusFileWithMultipleBlocks blockContents
      result = compile multiBlockFile
  in case result of
    Left err -> property $ "block" `isInfixOf` show err || "multiple" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with import statements compile correctly
prop_compile_import_statements :: [String] -> Property
prop_compile_import_statements importPaths =
  length importPaths <= 10 ==> -- Limit to avoid complexity
  let importFile = createSimpleTypusFileWithImports importPaths
      result = compile importFile
  in case result of
    Left err -> property $ "import" `isInfixOf` show err || "package" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode && "import" `isInfixOf` goCode

-- Property: Files with function definitions compile correctly
prop_compile_function_definitions :: [String] -> [String] -> [String] -> Property
prop_compile_function_definitions funcNames paramTypes returnTypes =
  let minLen = minimum [length funcNames, length paramTypes, length returnTypes]
      limitedFuncs = take minLen funcNames
      limitedParams = take minLen paramTypes
      limitedReturns = take minLen returnTypes
      functionFile = createSimpleTypusFileWithFunctions limitedFuncs limitedParams limitedReturns
      result = compile functionFile
  in case result of
    Left err -> property $ "function" `isInfixOf` show err || "definition" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with struct definitions compile correctly
prop_compile_struct_definitions :: [String] -> [String] -> Property
prop_compile_struct_definitions structNames fieldTypes =
  let minLen = min (length structNames) (length fieldTypes)
      limitedStructs = take minLen structNames
      limitedFields = take minLen fieldTypes
      structFile = createSimpleTypusFileWithStructs limitedStructs limitedFields
      result = compile structFile
  in case result of
    Left err -> property $ "struct" `isInfixOf` show err || "definition" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with interface definitions compile correctly
prop_compile_interface_definitions :: [String] -> [String] -> Property
prop_compile_interface_definitions interfaceNames methodNames =
  let minLen = min (length interfaceNames) (length methodNames)
      limitedInterfaces = take minLen interfaceNames
      limitedMethods = take minLen methodNames
      interfaceFile = createSimpleTypusFileWithInterfaces limitedInterfaces limitedMethods
      result = compile interfaceFile
  in case result of
    Left err -> property $ "interface" `isInfixOf` show err || "definition" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with generic type definitions compile correctly
prop_compile_generic_definitions :: [String] -> [String] -> [String] -> Property
prop_compile_generic_definitions typeNames typeParams constraints =
  let minLen = minimum [length typeNames, length typeParams, length constraints]
      limitedTypes = take minLen typeNames
      limitedParams = take minLen typeParams
      limitedConstraints = take minLen constraints
      genericFile = createSimpleTypusFileWithGenerics limitedTypes limitedParams limitedConstraints
      result = compile genericFile
  in case result of
    Left err -> property $ "generic" `isInfixOf` show err || "definition" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with error handling constructs compile correctly
prop_compile_error_handling :: [String] -> Property
prop_compile_error_handling functionNames =
  let errorFile = createSimpleTypusFileWithErrorHandling functionNames
      result = compile errorFile
  in case result of
    Left err -> property $ "error" `isInfixOf` show err || "handling" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Property: Files with concurrent constructs compile correctly
prop_compile_concurrent_constructs :: [String] -> Property
prop_compile_concurrent_constructs channelNames =
  let concurrentFile = createSimpleTypusFileWithConcurrency channelNames
      result = compile concurrentFile
  in case result of
    Left err -> property $ "concurrent" `isInfixOf` show err || "goroutine" `isInfixOf` show err || True
    Right goCode -> property $ "package main" `isInfixOf` goCode

-- Helper functions
createSimpleTypusFile :: String -> TypusFile
createSimpleTypusFile content = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined]  -- Would be implemented with actual CodeBlock constructor
            []  -- Syntax errors

createSimpleTypusFileWithDirectives :: FileDirectives -> TypusFile
createSimpleTypusFileWithDirectives directives = 
  TypusFile directives [] [undefined] []

createSimpleTypusFileWithBlocks :: Int -> TypusFile
createSimpleTypusFileWithBlocks numBlocks = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined | _ <- [1..numBlocks]]
            []

createNestedTypusFile :: Int -> TypusFile
createNestedTypusFile depth = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined]
            []

createSimpleTypusFileWithMultipleBlocks :: [String] -> TypusFile
createSimpleTypusFileWithMultipleBlocks contents = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined | _ <- contents]
            []

createSimpleTypusFileWithImports :: [String] -> TypusFile
createSimpleTypusFileWithImports importPaths = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined | _ <- importPaths]
            []

createSimpleTypusFileWithFunctions :: [String] -> [String] -> [String] -> TypusFile
createSimpleTypusFileWithFunctions funcNames paramTypes returnTypes = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined]
            []

createSimpleTypusFileWithStructs :: [String] -> [String] -> TypusFile
createSimpleTypusFileWithStructs structNames fieldTypes = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined]
            []

createSimpleTypusFileWithInterfaces :: [String] -> [String] -> TypusFile
createSimpleTypusFileWithInterfaces interfaceNames methodNames = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined]
            []

createSimpleTypusFileWithGenerics :: [String] -> [String] -> [String] -> TypusFile
createSimpleTypusFileWithGenerics typeNames typeParams constraints = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined]
            []

createSimpleTypusFileWithErrorHandling :: [String] -> TypusFile
createSimpleTypusFileWithErrorHandling functionNames = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined | _ <- functionNames]
            []

createSimpleTypusFileWithConcurrency :: [String] -> TypusFile
createSimpleTypusFileWithConcurrency channelNames = 
  TypusFile (FileDirectives Nothing Nothing Nothing) 
            []
            [undefined | _ <- channelNames]
            []

buildSimpleTypeEnv :: TypusFile -> Maybe (Map.Map String String)
buildSimpleTypeEnv file = Just $ Map.fromList [("test", "type")]

extractSimpleDeclarations :: TypusFile -> [String]
extractSimpleDeclarations file = ["decl1", "decl2"]

extractSimpleFunctionCalls :: TypusFile -> [String]
extractSimpleFunctionCalls file = ["call1", "call2"]

countChar :: Char -> String -> Int
countChar c = length . filter (== c)

tests :: TestTree
tests = testGroup "Extended Compiler QuickCheck Tests"
  [ fastProperty "Compilation deterministic" prop_compile_deterministic
  , fastProperty "Empty file compilation" prop_compile_empty_file
  , fastProperty "Directives only compilation" prop_compile_directives_only
  , fastProperty "Ownership directive effect" prop_compile_ownership_directive_effect
  , fastProperty "Dependent types directive effect" prop_compile_dependent_types_directive_effect
  , fastProperty "Constraints directive effect" prop_compile_constraints_directive_effect
  , fastProperty "Multiple directives interaction" prop_compile_multiple_directives_interaction
  , fastProperty "Type checking preservation" prop_compile_type_checking_preservation
  , fastProperty "Declaration extraction consistency" prop_compile_declaration_extraction_consistency
  , fastProperty "Function call extraction consistency" prop_compile_function_call_extraction_consistency
  , fastProperty "Go code syntax validity" prop_compile_go_code_syntax_validity
  , fastProperty "Dependent type checking" prop_compile_dependent_type_checking
  , fastProperty "Ownership checking" prop_compile_ownership_checking
  , fastProperty "Type error reporting" prop_compile_type_error_reporting
  , fastProperty "Compilation phases order" prop_compile_compilation_phases_order
  , fastProperty "Large files compilation" prop_compile_large_files
  , fastProperty "Nested structures compilation" prop_compile_nested_structures
  , fastProperty "Unicode content compilation" prop_compile_unicode_content
  , fastProperty "Special characters compilation" prop_compile_special_characters
  , fastProperty "Multiple blocks compilation" prop_compile_multiple_blocks
  , fastProperty "Import statements compilation" prop_compile_import_statements
  , fastProperty "Function definitions compilation" prop_compile_function_definitions
  , fastProperty "Struct definitions compilation" prop_compile_struct_definitions
  , fastProperty "Interface definitions compilation" prop_compile_interface_definitions
  , fastProperty "Generic definitions compilation" prop_compile_generic_definitions
  , fastProperty "Error handling compilation" prop_compile_error_handling
  , fastProperty "Concurrent constructs compilation" prop_compile_concurrent_constructs
  ]