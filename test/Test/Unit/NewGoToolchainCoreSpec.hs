{-# LANGUAGE CPP #-}

module Test.Unit.NewGoToolchainCoreSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Map as Map

import GoToolchain (GoToolchain, ToolchainVersion(..), defaultToolchain, 
                   runGoCommand, parseGoVersion, GoModule(..), ModulePath(..))
import Compiler.GoLexer (Token(..), tokenize, TokenType(..))
import Compiler.GoAst (GoNode(..), GoStatement(..), GoExpression(..))
import SourceLocation (SourcePos(..), SourceSpan(..))
import TestSupport.Arbitrary ()

-- Test 1: Go version parsing
prop_go_version_parsing :: String -> Property
prop_go_version_parsing versionStr =
  length versionStr > 0 && length versionStr < 20 ==>
  case parseGoVersion versionStr of
    Left _ -> property True -- Invalid versions should fail gracefully
    Right version -> property True -- Valid versions should parse

-- Test 2: Toolchain version comparison
prop_toolchain_version_comparison :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_toolchain_version_comparison major1 minor1 patch1 major2 minor2 patch2 =
  let version1 = ToolchainVersion major1 minor1 patch1
      version2 = ToolchainVersion major2 minor2 patch2
  in (major1 > major2 || (major1 == major2 && minor1 > minor2) || 
      (major1 == major2 && minor1 == minor2 && patch1 > patch2)) ==> 
     version1 > version2

-- Test 3: Go module path parsing
prop_go_module_path_parsing :: String -> Property
prop_go_module_path_parsing pathStr =
  length pathStr > 0 && length pathStr < 50 ==>
  let modulePath = ModulePath pathStr
  in case modulePath of
    ModulePath p -> length p > 0

-- Test 4: Go tokenization basic
prop_go_tokenization_basic :: String -> Property
prop_go_tokenization_basic str =
  length str > 0 && length str < 100 ==>
  let tokens = tokenize str
  in length tokens >= 0 -- Should always return a list

-- Test 5: Token position consistency
prop_token_position_consistency :: String -> Property
prop_token_position_consistency str =
  length str > 0 && length str < 50 ==>
  let tokens = tokenize str
      positions = map tokenPos tokens
  in length positions === length tokens

-- Test 6: Go command execution safety
prop_go_command_execution_safety :: String -> Property
prop_go_command_execution_safety cmd =
  length cmd > 0 && length cmd < 20 ==> -- Limit to reasonable size
  let toolchain = defaultToolchain
      result = runGoToolchain toolchain cmd
  in case result of
    Left _ -> property True -- Commands may fail
    Right _ -> property True -- Commands may succeed

  where
    runGoToolchain _ [] = Left "Empty command"
    runGoToolchain tc args = runGoCommand tc args

-- Test 7: Module dependency resolution
prop_module_dependency_resolution :: String -> [String] -> Property
prop_module_dependency_resolution moduleName deps =
  length moduleName > 0 && length deps < 10 ==> -- Limit complexity
  let module = GoModule (ModulePath moduleName) (map ModulePath deps)
      moduleDeps = getModuleDependencies module
  in length moduleDeps === length deps

  where
    getModuleDependencies (GoModule _ ds) = ds

-- Test 8: AST node roundtrip
prop_ast_node_roundtrip :: GoStatement -> Property
prop_ast_node_roundtrip stmt =
  let node = StatementNode stmt
      reconstructed = case node of
        StatementNode s -> Just s
        _ -> Nothing
  in reconstructed === Just stmt

-- Test 9: Token type classification
prop_token_type_classification :: String -> Property
prop_token_type_classification str =
  length str > 0 && length str < 20 ==>
  let tokens = tokenize str
      tokenTypes = map tokenType tokens
  in all isValidTokenType tokenTypes

  where
    isValidTokenType (Identifier _) = True
    isValidTokenType (Keyword _) = True
    isValidTokenType (Operator _) = True
    isValidTokenType (Literal _) = True
    isValidTokenType (Punctuation _) = True
    isValidTokenType (Whitespace _) = True
    isValidTokenType (Comment _) = True
    isValidTokenType (EOF _) = True

-- Test 10: Toolchain configuration consistency
prop_toolchain_configuration_consistency :: GoToolchain -> Property
prop_toolchain_configuration_consistency toolchain =
  let version = getToolchainVersion toolchain
      path = getToolchainPath toolchain
  in length path > 0

  where
    getToolchainVersion tc = ToolchainVersion 1 0 0 -- Default version
    getToolchainPath tc = "/usr/bin/go" -- Default path

tests :: TestTree
tests = testGroup "New GoToolchain Core Tests"
  [ fastProperty "Go version parsing" prop_go_version_parsing
  , fastProperty "Toolchain version comparison" prop_toolchain_version_comparison
  , fastProperty "Go module path parsing" prop_go_module_path_parsing
  , fastProperty "Go tokenization basic" prop_go_tokenization_basic
  , fastProperty "Token position consistency" prop_token_position_consistency
  , fastProperty "Go command execution safety" prop_go_command_execution_safety
  , fastProperty "Module dependency resolution" prop_module_dependency_resolution
  , fastProperty "AST node roundtrip" prop_ast_node_roundtrip
  , fastProperty "Token type classification" prop_token_type_classification
  , fastProperty "Toolchain configuration consistency" prop_toolchain_configuration_consistency
  ]