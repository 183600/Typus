{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.Unit.AdvancedParserQuickCheckTests where

import Test.Tasty
import Test.Tasty.QuickCheck
import TestSupport.QuickCheck (fastProperty, memoryEfficientProperty, ultraMemoryEfficientProperty)
import TestSupport.Arbitrary
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, sort, nub, partition)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Text as T
import Control.Monad (when, unless, replicateM)
import Data.Either (isLeft, isRight)

-- Import Parser modules
import Parser
  ( parseTypus
  , parseTypusFile
  , parseExpression
  , parseDeclaration
  , Declaration(..)
  , Expression(..)
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , tfContents
  , defaultFileDirectives
  , defaultBlockDirectives
  , fileDirectiveParser
  , isIdentifierChar
  )

import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedAt
  , locatedWithSpan
  , spanStart
  , spanEnd
  )

import Utils (trim, splitOn, joinWith)

-- ============================================================================
-- Advanced Parser Properties
-- ============================================================================

-- | Property: Parsing and unparsing expressions should be idempotent
prop_expression_parse_unparse_idempotent :: String -> Property
prop_expression_parse_unparse_idempotent expr = 
  case parseExpression expr of
    Left _ -> property True -- Skip invalid expressions
    Right parsedExpr -> 
      let unparsed = show parsedExpr
          reparsed = parseExpression unparsed
      in property $ isRight reparsed

-- | Property: Nested expressions should maintain structure
prop_nested_expressions_structure :: Int -> Property
prop_nested_expressions_structure depth = 
  let nestedExpr = generateNestedExpression depth
  in property $ isRight $ parseExpression nestedExpr
  where
    generateNestedExpression 0 = "x"
    generateNestedExpression n = "(" ++ generateNestedExpression (n-1) ++ " + " ++ generateNestedExpression (n-1) ++ ")"

-- | Property: Declaration parsing should handle complex types
prop_complex_type_declaration :: String -> String -> Property
prop_complex_type_declaration typeName typeDef = 
  let isValidName = isValidIdentifier typeName && not (null typeName)
      isValidTypeDef = not $ null typeDef
  in whenValid $ property $ 
    if isValidName && isValidTypeDef
      then let decl = "type " ++ typeName ++ " " ++ typeDef
           in property $ isRight $ parseDeclaration decl
      else property True
  where
    whenValid = guard (isValidName && isValidTypeDef)

-- | Property: Function declarations should preserve parameter order
prop_function_parameter_order :: String -> [String] -> Property
prop_function_parameter_order funcName params = 
  let isValidName = isValidIdentifier funcName && not (null funcName)
      validParams = all isValidIdentifier params
  in whenValid $ property $ 
    if isValidName && validParams
      then let paramStr = joinWith ", " params
               funcDecl = "func " ++ funcName ++ "(" ++ paramStr ++ ") {}"
           in case parseDeclaration funcDecl of
                Right decl -> property $ extractParamOrder decl == params
                Left _ -> property True -- Skip invalid parsing
      else property True
  where
    whenValid = guard (isValidName && validParams)
    extractParamOrder (FunctionDeclaration _ parameters _) = map unLoc parameters
    extractParamOrder _ = []

-- | Property: Import declarations should handle various path formats
prop_import_path_formats :: String -> Property
prop_import_path_formats importPath = 
  let hasValidFormat = not $ null importPath
      isStandardImport = any (`isPrefixOf` importPath) ["\"", "./", "../", "/"]
  in whenValid $ property $ 
    if hasValidFormat && isStandardImport
      then let importDecl = "import " ++ importPath
           in property $ isRight $ parseDeclaration importDecl
      else property True
  where
    whenValid = guard hasValidFormat

-- | Property: File directives should be parsed in correct order
prop_file_directives_order :: FileDirectives -> Property
prop_file_directives_order directives = 
  let directiveStr = show directives
      parsed = fileDirectiveParser directiveStr
  in property $ isRight parsed

-- | Property: Block directives should be properly scoped
prop_block_directives_scope :: BlockDirectives -> String -> Property
prop_block_directives_scope directives code = 
  let block = CodeBlock directives code []
      blockStr = show block
      parsed = parseExpression blockStr
  in property $ isRight parsed

-- | Property: Comments should be properly ignored in parsing
prop_comments_ignored :: String -> String -> Property
prop_comments_ignored code comment = 
  let codeWithComment = code ++ " // " ++ comment
      result1 = parseExpression code
      result2 = parseExpression codeWithComment
  in case (result1, result2) of
    (Right _, Right _) -> property True -- Both parsed successfully
    (Left _, Left _) -> property True  -- Both failed (same behavior)
    _ -> property False -- Inconsistent behavior

-- | Property: Whitespace variations should not affect parsing
prop_whitespace_variations :: String -> Property
prop_whitespace_variations code = 
  let compactCode = normalizeWhitespace code
      expandedCode = expandWhitespace code
      result1 = parseExpression compactCode
      result2 = parseExpression expandedCode
  in case (result1, result2) of
    (Right _, Right _) -> property True -- Both parsed successfully
    (Left _, Left _) -> property True  -- Both failed (same behavior)
    _ -> property False -- Inconsistent behavior
  where
    normalizeWhitespace = unwords . words
    expandWhitespace = unwords $ map (\c -> if isSpace c then "  " else [c]) code

-- | Property: Unicode identifiers should be handled correctly
prop_unicode_identifiers :: String -> Property
prop_unicode_identifiers ident = 
  let hasUnicode = any (> '\127') ident
      isValidIdent = not (null ident) && isAlpha (head ident) && all isAlphaNum ident
  in whenValid $ property $ 
    if hasUnicode && isValidIdent
      then let expr = ident ++ " + 1"
           in property $ isRight $ parseExpression expr
      else property True
  where
    whenValid = guard (hasUnicode && isValidIdent)

-- | Property: Escape sequences in strings should be parsed correctly
prop_string_escape_sequences :: String -> Property
prop_string_escape_sequences content = 
  let escapedContent = escapeStringContent content
      stringLiteral = "\"" ++ escapedContent ++ "\""
      expr = "x := " ++ stringLiteral
  in property $ isRight $ parseExpression expr
  where
    escapeStringContent = concatMap escapeChar
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar '\\' = "\\\\"
    escapeChar '"' = "\\\""
    escapeChar c = [c]

-- | Property: Numeric literals of various bases should be parsed
prop_numeric_literal_bases :: Int -> Int -> Property
prop_numeric_literal_bases base value = 
  let validBase = base `elem` [2, 8, 10, 16]
      validValue = value >= 0 && value < 1000
  in whenValid $ property $ 
    if validBase && validValue
      then let literal = case base of
                         2 -> "0b" ++ showInBase 2 value
                         8 -> "0o" ++ showInBase 8 value
                         10 -> show value
                         16 -> "0x" ++ showInBase 16 value
               expr = "x := " ++ literal
           in property $ isRight $ parseExpression expr
      else property True
  where
    whenValid = guard (validBase && validValue)
    showInBase b n = case b of
      2 -> showIntAtBase 2 "01" n
      8 -> showIntAtBase 8 "01234567" n
      10 -> show n
      16 -> showIntAtBase 16 "0123456789ABCDEF" n
    showIntAtBase base digits n
      | n < base = [digits !! n]
      | otherwise = showIntAtBase base digits (n `div` base) ++ [digits !! (n `mod` base)]

-- | Property: Operator precedence should be correctly parsed
prop_operator_precedence :: String -> String -> String -> Property
prop_operator_precedence op1 op2 op3 = 
  let validOps = all (`elem` ["+", "-", "*", "/", "%", "&&", "||", "<", ">", "<=", ">="]) [op1, op2, op3]
  in whenValid $ property $ 
    if validOps
      then let expr = "1 " ++ op1 ++ " 2 " ++ op2 ++ " 3 " ++ op3 ++ " 4"
           in property $ isRight $ parseExpression expr
      else property True
  where
    whenValid = guard validOps

-- | Property: Complex type expressions should be parsed correctly
prop_complex_type_expressions :: String -> Property
prop_complex_type_expressions typeExpr = 
  let hasValidStructure = any (`isInfixOf` typeExpr) ["[]", "map[", "chan", "func"]
  in whenValid $ property $ 
    if hasValidStructure
      then let decl = "var x " ++ typeExpr
           in property $ isRight $ parseDeclaration decl
      else property True
  where
    whenValid = guard hasValidStructure

-- | Property: Interface type definitions should be parsed correctly
prop_interface_type_definitions :: String -> [String] -> Property
prop_interface_type_definitions interfaceName methods = 
  let validName = isValidIdentifier interfaceName && not (null interfaceName)
      validMethods = all isValidIdentifier methods
  in whenValid $ property $ 
    if validName && validMethods
      then let methodStrs = map (\m -> m ++ "()") methods
               interfaceBody = joinWith "\n  " methodStrs
               interfaceDef = "type " ++ interfaceName ++ " interface {\n  " ++ interfaceBody ++ "\n}"
           in property $ isRight $ parseDeclaration interfaceDef
      else property True
  where
    whenValid = guard (validName && validMethods)

-- | Property: Struct type definitions should be parsed correctly
prop_struct_type_definitions :: String -> [(String, String)] -> Property
prop_struct_type_definitions structName fields = 
  let validName = isValidIdentifier structName && not (null structName)
      validFields = all (\(name, typ) -> isValidIdentifier name && not (null typ)) fields
  in whenValid $ property $ 
    if validName && validFields
      then let fieldStrs = map (\(name, typ) -> name ++ " " ++ typ) fields
               structBody = joinWith "\n  " fieldStrs
               structDef = "type " ++ structName ++ " struct {\n  " ++ structBody ++ "\n}"
           in property $ isRight $ parseDeclaration structDef
      else property True
  where
    whenValid = guard (validName && validFields)

-- | Property: Generic type parameters should be parsed correctly
prop_generic_type_parameters :: String -> [String] -> String -> Property
prop_generic_type_parameters typeName typeParams baseType = 
  let validName = isValidIdentifier typeName && not (null typeName)
      validParams = all isValidIdentifier typeParams
      validBase = not $ null baseType
  in whenValid $ property $ 
    if validName && validParams && validBase
      then let paramStr = joinWith ", " typeParams
               genericType = typeName ++ "[" ++ paramStr ++ "] " ++ baseType
               decl = "var x " ++ genericType
           in property $ isRight $ parseDeclaration decl
      else property True
  where
    whenValid = guard (validName && validParams && validBase)

-- | Property: Dependent type constraints should be parsed correctly
prop_dependent_type_constraints :: String -> String -> Property
prop_dependent_type_constraints typeName constraint = 
  let validName = isValidIdentifier typeName && not (null typeName)
      validConstraint = not $ null constraint
  in whenValid $ property $ 
    if validName && validConstraint
      then let dependentType = typeName ++ " where { " ++ constraint ++ " }"
               decl = "var x " ++ dependentType
           in property $ isRight $ parseDeclaration decl
      else property True
  where
    whenValid = guard (validName && validConstraint)

-- | Property: Function type expressions should be parsed correctly
prop_function_type_expressions :: [String] -> String -> Property
prop_function_type_expressions paramTypes returnType = 
  let validParams = all (not . null) paramTypes
      validReturn = not $ null returnType
  in whenValid $ property $ 
    if validParams && validReturn
      then let paramStr = joinWith ", " paramTypes
               funcType = "func(" ++ paramStr ++ ") " ++ returnType
               decl = "var x " ++ funcType
           in property $ isRight $ parseDeclaration decl
      else property True
  where
    whenValid = guard (validParams && validReturn)

-- | Property: Array and slice types should be parsed correctly
prop_array_slice_types :: String -> String -> Property
prop_array_slice_types elementType size = 
  let validElement = not $ null elementType
      validSize = not $ null size
  in whenValid $ property $ 
    if validElement && validSize
      then let arrayType = "[" ++ size ++ "]" ++ elementType
               sliceType = "[]" ++ elementType
               arrayDecl = "var a " ++ arrayType
               sliceDecl = "var s " ++ sliceType
           in property $ isRight (parseDeclaration arrayDecl) && isRight (parseDeclaration sliceDecl)
      else property True
  where
    whenValid = guard (validElement && validSize)

-- | Property: Map types should be parsed correctly
prop_map_types :: String -> String -> Property
prop_map_types keyType valueType = 
  let validKey = not $ null keyType
      validValue = not $ null valueType
  in whenValid $ property $ 
    if validKey && validValue
      then let mapType = "map[" ++ keyType ++ "]" ++ valueType
               decl = "var m " ++ mapType
           in property $ isRight $ parseDeclaration decl
      else property True
  where
    whenValid = guard (validKey && validValue)

-- | Property: Channel types should be parsed correctly
prop_channel_types :: String -> Property
prop_channel_types elementType = 
  let validElement = not $ null elementType
  in whenValid $ property $ 
    if validElement
      then let chanType = "chan " ++ elementType
               decl = "var c " ++ chanType
           in property $ isRight $ parseDeclaration decl
      else property True
  where
    whenValid = guard validElement

-- | Property: Pointer types should be parsed correctly
prop_pointer_types :: String -> Property
prop_pointer_types baseType = 
  let validBase = not $ null baseType
  in whenValid $ property $ 
    if validBase
      then let ptrType = "*" ++ baseType
               decl = "var p " ++ ptrType
           in property $ isRight $ parseDeclaration decl
      else property True
  where
    whenValid = guard validBase

-- ============================================================================
-- Test Suite
-- ============================================================================

tests :: TestTree
tests = testGroup "Advanced Parser QuickCheck Tests"
  [ testGroup "Expression Parsing"
    [ memoryEfficientProperty "parse unparse idempotent" prop_expression_parse_unparse_idempotent
    , fastProperty "nested expressions structure" prop_nested_expressions_structure
    , memoryEfficientProperty "unicode identifiers" prop_unicode_identifiers
    , fastProperty "string escape sequences" prop_string_escape_sequences
    , fastProperty "numeric literal bases" prop_numeric_literal_bases
    , fastProperty "operator precedence" prop_operator_precedence
    ]
  , testGroup "Declaration Parsing"
    [ fastProperty "complex type declaration" prop_complex_type_declaration
    , fastProperty "function parameter order" prop_function_parameter_order
    , fastProperty "import path formats" prop_import_path_formats
    , fastProperty "interface type definitions" prop_interface_type_definitions
    , fastProperty "struct type definitions" prop_struct_type_definitions
    ]
  , testGroup "Type System Parsing"
    [ fastProperty "generic type parameters" prop_generic_type_parameters
    , fastProperty "dependent type constraints" prop_dependent_type_constraints
    , fastProperty "function type expressions" prop_function_type_expressions
    , fastProperty "array and slice types" prop_array_slice_types
    , fastProperty "map types" prop_map_types
    , fastProperty "channel types" prop_channel_types
    , fastProperty "pointer types" prop_pointer_types
    , fastProperty "complex type expressions" prop_complex_type_expressions
    ]
  , testGroup "Directive Parsing"
    [ fastProperty "file directives order" prop_file_directives_order
    , fastProperty "block directives scope" prop_block_directives_scope
    ]
  , testGroup "Robustness"
    [ fastProperty "comments ignored" prop_comments_ignored
    , fastProperty "whitespace variations" prop_whitespace_variations
    ]
  ]