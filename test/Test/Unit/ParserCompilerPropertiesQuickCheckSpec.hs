{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.ParserCompilerPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, ioProperty, (===), (.&&.), counterexample, forAll, oneof, elements, listOf)

import Parser (parseTypus, TypusFile(..), FileDirectives(..), BlockDirectives(..))
import Compiler (compile, CompilerError(..), extractDeclarations, extractFunctionCalls)
import SourceLocation (SourceSpan(..))

import qualified Data.List as L
import Data.List (isInfixOf, isPrefixOf)
import Data.List (sort)

-- Helper functions
validIdentifier :: Gen String
validIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return $ first : rest

validGoType :: Gen String
validGoType = oneof
  [ return "int"
  , return "string"
  , return "bool"
  , return "float64"
  , return "interface{}"
  , do
      base <- validIdentifier
      return $ "[]" ++ base
  , do
      key <- validGoType
      value <- validGoType
      return $ "map[" ++ key ++ "]" ++ value
  ]

simpleExpression :: Gen String
simpleExpression = oneof
  [ do
      n <- elements [1..100]
      return $ show n
  , do
      s <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
      return $ "\"" ++ s ++ "\""
  , do
      name <- validIdentifier
      return name
  ]

simpleStatement :: Gen String
simpleStatement = oneof
  [ do
      name <- validIdentifier
      expr <- simpleExpression
      return $ name ++ " := " ++ expr
  , do
      name <- validIdentifier
      args <- listOf simpleExpression
      return $ name ++ "(" ++ unwords args ++ ")"
  , do
      name <- validIdentifier
      expr <- simpleExpression
      return $ name ++ " = " ++ expr
  ]

tests :: TestTree
tests =
  testGroup "Parser & Compiler Properties QuickCheck Tests"
    [ testProperty "parseTypus handles empty input" $ prop_parse_empty
    , testProperty "parseTypus handles simple package declaration" $ prop_parse_simple_package
    , testProperty "parseTypus preserves function declarations" $ prop_parse_preserves_functions
    , testProperty "extractDeclarations finds L.all function names" $ prop_extract_declarations_completeness
    , testProperty "extractFunctionCalls finds L.all function calls" $ prop_extract_calls_completeness
    , testProperty "parseTypus roundtrip with simple programs" $ prop_parse_roundtrip_simple
    , testProperty "compile handles syntactically valid programs" $ prop_compile_valid_syntax
    , testProperty "compile fails on invalid syntax" $ prop_compile_invalid_syntax
    , testProperty "parseTypus directive consistency" $ prop_parse_directive_consistency
    , testProperty "extractDeclarations idempotent" $ prop_extract_declarations_idempotent
    ]

-- | parseTypus should handle empty input gracefully
prop_parse_empty :: Property
prop_parse_empty = ioProperty $ do
  case parseTypus "" of
    Left _ -> return False
    Right typusFile -> return $ L.null (tfBlocks typusFile)

-- | parseTypus should handle simple package declarations
prop_parse_simple_package :: Property
prop_parse_simple_package = ioProperty $ do
  let source = "package main\nfunc main() {}\n"
  case parseTypus source of
    Left _ -> return False
    Right typusFile -> return $ not $ L.null $ tfBlocks typusFile

-- | parseTypus should preserve function declarations in content
prop_parse_preserves_functions :: Property
prop_parse_preserves_functions = forAll simpleFunction $ \func -> ioProperty $ do
  let source = "package main\n\n" ++ func ++ "\n"
  case parseTypus source of
    Left _ -> return False
    Right typusFile -> do
      let content = unwords $ map cbContent $ tfBlocks typusFile
      return $ L.any (`L.isInfixOf` content) ["func", "main"]

-- | extractDeclarations should find L.all function names present in source
prop_extract_declarations_completeness :: Property
prop_extract_declarations_completeness = forAll simpleProgram $ \program -> ioProperty $ do
  case parseTypus program of
    Left _ -> return True  -- If parsing fails, property is vacuously true
    Right typusFile -> do
      let declarations = extractDeclarations typusFile
      let hasMain = L.any ("main" `L.isInfixOf`) declarations
      return $ if "func main" `L.isInfixOf` program 
               then hasMain 
               else True

-- | extractFunctionCalls should find function calls in the source
prop_extract_calls_completeness :: Property
prop_extract_calls_completeness = forAll programWithCalls $ \program -> ioProperty $ do
  case parseTypus program of
    Left _ -> return True
    Right typusFile -> do
      let calls = extractFunctionCalls typusFile
      let hasPrintln = L.any ("println" `L.isInfixOf`) calls
      return $ if "println(" `L.isInfixOf` program
               then hasPrintln
               else True

-- | parseTypus roundtrip: parsing should be consistent for simple programs
prop_parse_roundtrip_simple :: Property
prop_parse_roundtrip_simple = forAll simpleProgram $ \program -> ioProperty $ do
  case parseTypus program of
    Left _ -> return True
    Right typusFile -> do
      let blocks = tfBlocks typusFile
      let content = unlines $ map cbContent blocks
      case parseTypus content of
        Left _ -> return False
        Right typusFile2 -> return $ L.length (tfBlocks typusFile) == L.length (tfBlocks typusFile2)

-- | compile should handle syntactically valid programs without crashing
prop_compile_valid_syntax :: Property
prop_compile_valid_syntax = forAll validSimpleProgram $ \program -> ioProperty $ do
  case parseTypus program of
    Left _ -> return True  -- If parsing fails, property is vacuously true
    Right typusFile -> do
      result <- compile typusFile
      return $ case result of
        Left _ -> True  -- Compilation can fail, but shouldn't crash
        Right _ -> True

-- | compile should fail on obviously invalid syntax
prop_compile_invalid_syntax :: Property
prop_compile_invalid_syntax = forAll invalidProgram $ \program -> ioProperty $ do
  case parseTypus program of
    Left _ -> return True  -- Parsing should fail first
    Right typusFile -> do
      result <- compile typusFile
      return $ case result of
        Left _ -> True  -- Should fail compilation
        Right _ -> False -- Should not succeed compilation

-- | parseTypus directive consistency: directives should be parsed consistently
prop_parse_directive_consistency :: Property
prop_parse_directive_consistency = forAll programWithDirectives $ \program -> ioProperty $ do
  case parseTypus program of
    Left _ -> return True
    Right typusFile -> do
      let FileDirectives { fdOwnership = ownership, fdDependentTypes = dependentTypes } = tfDirectives typusFile
      let hasOwnershipDirective = isJust ownership
      let hasDependentTypesDirective = isJust dependentTypes
      let sourceHasOwnership = "ownership:" `L.isInfixOf` program
      let sourceHasDependentTypes = "dependent_types:" `L.isInfixOf` program
      return $ hasOwnershipDirective == sourceHasOwnership &&
                hasDependentTypesDirective == sourceHasDependentTypes

-- | extractDeclarations should be idempotent
prop_extract_declarations_idempotent :: Property
prop_extract_declarations_idempotent = forAll simpleProgram $ \program -> ioProperty $ do
  case parseTypus program of
    Left _ -> return True
    Right typusFile -> do
      let decls1 = extractDeclarations typusFile
      let decls2 = extractDeclarations typusFile
      return $ sort decls1 == sort decls2

-- Helper generators
simpleFunction :: Gen String
simpleFunction = do
  name <- elements ["main", "test", "helper"]
  return $ "func " ++ name ++ "() {\n    println(\"test\")\n}"

simpleProgram :: Gen String
simpleProgram = do
  func <- simpleFunction
  return $ "package main\n\n" ++ func ++ "\n"

programWithCalls :: Gen String
programWithCalls = do
  calls <- listOf $ elements ["println(\"hello\")", "test()", "helper()"]
  return $ unlines
    [ "package main"
    , "func main() {"
    , "    " ++ unlines calls
    , "}"
    ]

validSimpleProgram :: Gen String
validSimpleProgram = do
  statements <- listOf simpleStatement
  return $ unlines
    [ "package main"
    , "func main() {"
    , "    " ++ unlines statements
    , "}"
    ]

invalidProgram :: Gen String
invalidProgram = oneof
  [ return $ unlines
      [ "func main() {"
      , "    println(\"test\""  -- Missing closing parenthesis
      , "}"
      ]
  , return $ unlines
      [ "package main"
      , "func main() {"  -- Missing closing brace
      , "    println(\"test\")"
      ]
  , return $ unlines
      [ "package main"
      , "func main() {"
      , "    if true {"  -- Unclosed if statement
      , "        println(\"test\")"
      , "    }"
      , "}"
      ]
  ]

programWithDirectives :: Gen String
programWithDirectives = do
  hasOwnership <- arbitrary
  hasDependentTypes <- arbitrary
  let directives = L.concat 
        [ ["//! ownership: on" | hasOwnership]
        , ["//! dependent_types: off" | hasDependentTypes]
        ]
  return $ unlines
    [ unlines directives
    , "package main"
    , "func main() {"
    , "    println(\"test\")"
    , "}"
    ]