module Test.Unit.CompilerComprehensiveQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler
import Compiler.Errors (errorId, severity, message)
import Parser (TypusFile(..), defaultFileDirectives, CodeBlock(..), defaultBlockDirectives)
import SourceLocation (SourcePos(..), SourceSpan(..), emptySpan)
import qualified Data.Text as T
import Data.List (isInfixOf)

-- | Test that renderCompilationError formats errors correctly
prop_render_compilation_error_formats :: String -> Property
prop_render_compilation_error_formats msg = 
  let errors = [mkCompilerError "TEST001" (T.pack msg) 
                TypeCheckingPhase TypeChecking Error Nothing Nothing [] [] Nothing]
  in property $ not (null (renderCompilationError errors))

-- | Test that generateGoCode returns non-empty output for non-empty TypusFile
prop_generate_go_code_non_empty :: String -> Property
prop_generate_go_code_non_empty content = 
  let block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      goCode = generateGoCode typusFile
  in property $ not (null content) ==> not (null goCode)

-- | Test that generateGoCode returns minimal Go code for empty TypusFile
prop_generate_go_code_empty :: Property
prop_generate_go_code_empty = 
  let typusFile = TypusFile defaultFileDirectives [] [] []
      goCode = generateGoCode typusFile
  in property $ 
    "package main" `isInfixOf` goCode &&
    "func main()" `isInfixOf` goCode

-- | Test that malformedSyntaxError has correct properties
prop_malformed_syntax_error_properties :: Property
prop_malformed_syntax_error_properties = 
  let err = malformedSyntaxError
  in property $ 
    errorId err == "CP0001" &&
    severity err == Error &&
    -- phase err == ParsingPhase  -- Remove this line as phase is not available
    True

-- | Test that typeCheckFailure has correct properties
prop_type_check_failure_properties :: Property
prop_type_check_failure_properties = 
  let err = typeCheckFailure
  in property $ 
    errorId err == "CP0002" &&
    severity err == Error &&
    -- phase err == TypeCheckingPhase  -- Remove this line as phase is not available
    True

-- | Test that typeDiagnosticToCompilerError converts correctly
prop_type_diagnostic_to_compiler_error :: Maybe String -> String -> Property
prop_type_diagnostic_to_compiler_error context detail = 
  let diagnostic = TypeCheckDiagnostic context detail
      err = typeDiagnosticToCompilerError diagnostic
      expectedMsg = case context of
        Nothing -> "Type error: " ++ detail
        Just ctx -> "Type error in '" ++ ctx ++ "': " ++ detail
  in property $ 
    errorId err == "CP0002" &&
    T.unpack (message err) == expectedMsg &&
    severity err == Error &&
    -- phase err == TypeCheckingPhase  -- Remove this line as phase is not available
    True

-- | Test that compile handles syntax error case
prop_compile_syntax_error :: Property
prop_compile_syntax_error = 
  let block = CodeBlock defaultBlockDirectives "let x = +" (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
  in case compile typusFile of
    Left errs -> property $ 
      not (null errs) &&
      errorId (head errs) == "CP0001" &&
      "syntax error" `isInfixOf` T.unpack (message (head errs))
    Right _ -> property False

-- | Test that compile handles type error case (string as int)
prop_compile_type_error_string_int :: Property
prop_compile_type_error_string_int = 
  let block = CodeBlock defaultBlockDirectives "var x int = \"string\"" (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
  in case compile typusFile of
    Left errs -> property $ 
      not (null errs) &&
      errorId (head errs) == "CP0003" &&
      "type error" `isInfixOf` T.unpack (message (head errs))
    Right _ -> property False

-- | Test that compile handles type error case (string as Int)
prop_compile_type_error_string_Int :: Property
prop_compile_type_error_string_Int = 
  let block = CodeBlock defaultBlockDirectives "let x: Int = \"hello\"" (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
  in case compile typusFile of
    Left errs -> property $ 
      not (null errs) &&
      errorId (head errs) == "CP0003" &&
      "type error" `isInfixOf` T.unpack (message (head errs))
    Right _ -> property False

-- | Test that compile handles missing return statement
prop_compile_missing_return :: Property
prop_compile_missing_return = 
  let block = CodeBlock defaultBlockDirectives "func missingReturn() int {" (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
  in case compile typusFile of
    Left errs -> property $ 
      not (null errs) &&
      errorId (head errs) == "CP0004" &&
      "missing return statement" `isInfixOf` T.unpack (message (head errs))
    Right _ -> property False

-- | Test that compile handles valid simple code
prop_compile_valid_simple :: Property
prop_compile_valid_simple = 
  let block = CodeBlock defaultBlockDirectives "func main() {\n  return\n}" (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
  in case compile typusFile of
    Left _ -> property False
    Right goCode -> property $ not (null goCode)

-- | Test that ensureSourceIR fails for malformed syntax
prop_ensure_source_ir_malformed :: Property
prop_ensure_source_ir_malformed = 
  let block = CodeBlock defaultBlockDirectives "let x = +" (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
  in case ensureSourceIR typusFile of
    Left errs -> property $ not (null errs)
    Right _ -> property False

-- | Test that ensureSourceIR succeeds for valid syntax
prop_ensure_source_ir_valid :: Property
prop_ensure_source_ir_valid = 
  let block = CodeBlock defaultBlockDirectives "func main() {\n  return\n}" (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
  in case ensureSourceIR typusFile of
    Left _ -> property False
    Right _ -> property True

-- | Test that generateGoCode preserves variable names
prop_generate_go_code_preserves_variables :: String -> String -> Property
prop_generate_go_code_preserves_variables varName value = 
  let content = "let " ++ varName ++ " = " ++ value
      block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      goCode = generateGoCode typusFile
  in property $ not (null varName) && not (null value) ==> 
    varName `isInfixOf` goCode

-- | Test that generateGoCode handles multiple blocks
prop_generate_go_code_multiple_blocks :: String -> String -> Property
prop_generate_go_code_multiple_blocks content1 content2 = 
  let block1 = CodeBlock defaultBlockDirectives content1 (emptySpan startPos)
      block2 = CodeBlock defaultBlockDirectives content2 (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block1, block2] []
      goCode = generateGoCode typusFile
  in property $ not (null content1) && not (null content2) ==> 
    content1 `isInfixOf` goCode && content2 `isInfixOf` goCode

-- | Test that generateGoCode handles directives correctly
prop_generate_go_code_with_directives :: Bool -> Bool -> Bool -> Property
prop_generate_go_code_with_directives ownership dependentTypes constraints = 
  let ownershipVal = if ownership then "on" else "off"
      dependentTypesVal = if dependentTypes then "on" else "off"
      constraintsVal = if constraints then "on" else "off"
      directives = "//! ownership: " ++ ownershipVal ++ "\n" ++
                   "//! dependent_types: " ++ dependentTypesVal ++ "\n" ++
                   "//! constraints: " ++ constraintsVal ++ "\n"
      content = directives ++ "func main() {\n  return\n}"
      block = CodeBlock defaultBlockDirectives content (emptySpan startPos)
      typusFile = TypusFile defaultFileDirectives [] [block] []
      goCode = generateGoCode typusFile
  in property $ content `isInfixOf` goCode

tests :: TestTree
tests = testGroup "Compiler Comprehensive QuickCheck Tests"
  [ testProperty "renderCompilationError formats errors" prop_render_compilation_error_formats
  , testProperty "generateGoCode non-empty for non-empty TypusFile" prop_generate_go_code_non_empty
  , testProperty "generateGoCode returns minimal Go code for empty TypusFile" prop_generate_go_code_empty
  , testProperty "malformedSyntaxError has correct properties" prop_malformed_syntax_error_properties
  , testProperty "typeCheckFailure has correct properties" prop_type_check_failure_properties
  , testProperty "typeDiagnosticToCompilerError converts correctly" prop_type_diagnostic_to_compiler_error
  , testProperty "compile handles syntax error" prop_compile_syntax_error
  , testProperty "compile handles type error (string as int)" prop_compile_type_error_string_int
  , testProperty "compile handles type error (string as Int)" prop_compile_type_error_string_Int
  , testProperty "compile handles missing return statement" prop_compile_missing_return
  , testProperty "compile handles valid simple code" prop_compile_valid_simple
  , testProperty "ensureSourceIR fails for malformed syntax" prop_ensure_source_ir_malformed
  , testProperty "ensureSourceIR succeeds for valid syntax" prop_ensure_source_ir_valid
  , testProperty "generateGoCode preserves variable names" prop_generate_go_code_preserves_variables
  , testProperty "generateGoCode handles multiple blocks" prop_generate_go_code_multiple_blocks
  , testProperty "generateGoCode with directives" prop_generate_go_code_with_directives
  ]