{-# LANGUAGE CPP #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Additional QuickCheck Arbitrary instances for extended testing
module TestSupport.ExtendedArbitrary where

import TestSupport.Arbitrary () -- Import base instances

import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, sized, frequency, choose, vectorOf, suchThat, property, resize)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (isPrefixOf, isInfixOf, nub)
import Data.Char (isAlphaNum, isSpace, isLower, isUpper)

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  )
import SyntaxValidator
  ( SyntaxError(..)
  , ErrorType(..)
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  )

import Compiler.GoAst
  ( GoModule(..)
  , GoDecl(..)
  , ImportDecl(..)
  , FuncDecl(..)
  , TypeDecl(..)
  , VarDecl(..)
  , ConstDecl(..)
  , PackageDecl(..)
  )

import Compiler.TypeChecker
  ( Type(..)
  , TypeEnv(..)
  , FunctionParam(..)
  , FunctionSignature(..)
  , CallExpr(..)
  )

import Analyzer.Types
  ( SymbolInfo(..)
  , SymbolKind(..)
  , AnalysisResult(..)
  , AnalysisPhase(..)
  , AnalysisContext(..)
  , AnalyzerState(..)
  , CombinedError(..)
  )

import Compiler.Errors.Core
  ( ErrorSeverity(..), CombinedError(..), ErrorLocation(..)
  )

import Ownership 
  ( OwnershipType(..)
  , OwnershipError(..)
  , OwnershipTransfer(..)
  , OwnershipAnalyzer(..)
  , newOwnershipAnalyzer
  )

import Dependencies.AST
  ( AST(..)
  , Statement(..)
  , TypeExpr(..)
  , DependencyGraph(..)
  , DependencyNode(..)
  )
import qualified Dependencies.AST as Dep

import Dependencies.TypeSystem
  ( TypeConstraint(..)
  , TypeVar(..)
  , DependentTypeError(..)
  , TypeDef(..)
  , TypeEnv(..)
  , DependentTypeChecker(..)
  , newDependentTypeChecker
  )

import Dependencies.Inference
  ( TypeScheme(..)
  )

-- Add T.Text Arbitrary instance
instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

























instance Arbitrary ErrorType where
  arbitrary = elements
    [ MissingBrace
    , MissingParenthesis
    , MissingBracket
    , UnclosedString
    , UnclosedComment
    , InvalidIdentifier
    , InvalidTypeDeclaration
    , InvalidFunctionDeclaration
    , InvalidImport
    , InvalidStatement
    , UnterminatedBlock
    , InvalidOperator
    , MissingSemicolon
    , UnexpectedToken
    , MissingPackageDeclaration
    , DuplicateDeclaration
    , InvalidBlockStructure
    , UndeclaredVariable
    ]





















instance Arbitrary OwnershipAnalyzer where



  arbitrary = return newOwnershipAnalyzer























instance Arbitrary SyntaxError where
  arbitrary = SyntaxError <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ErrorLocation where
  arbitrary = ErrorLocation <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Dep.Constraint where
  arbitrary = oneof
    [ Dep.SizeGT <$> arbitrary <*> arbitrary
    , Dep.SizeGE <$> arbitrary <*> arbitrary
    , Dep.RangeC <$> arbitrary <*> arbitrary <*> arbitrary
    , Dep.PredC <$> arbitrary <*> arbitrary
    ]



instance Arbitrary Dependencies.AST.TypeExpr where
  arbitrary = oneof
    [ Dependencies.AST.SimpleT <$> arbitrary
    , Dependencies.AST.GenericT <$> arbitrary <*> arbitrary
    ]











instance Arbitrary Dependencies.TypeSystem.TypeEnv where
  arbitrary = Dependencies.TypeSystem.TypeEnv <$> arbitrary <*> arbitrary



-- Add TypeDef Arbitrary instance
instance Arbitrary Dependencies.TypeSystem.TypeDef where
  arbitrary = Dependencies.TypeSystem.TypeDefDecl <$> arbitrary <*> arbitrary



instance Arbitrary DependentTypeChecker where
  arbitrary = return newDependentTypeChecker





instance Arbitrary OwnershipTransfer where
  arbitrary = OwnershipTransfer <$> arbitrary <*> arbitrary

instance Arbitrary DependencyNode where
  arbitrary = DependencyNode <$> arbitrary <*> arbitrary

instance Arbitrary Dependencies.AST.Statement where
  arbitrary = oneof
    [ Dependencies.AST.STypeDef <$> arbitrary <*> arbitrary <*> arbitrary
    , Dependencies.AST.STypeAlias <$> arbitrary <*> arbitrary <*> arbitrary
    , Dependencies.AST.SVarDecl <$> arbitrary <*> arbitrary
    ]

instance Arbitrary DependencyGraph where
  arbitrary = DependencyGraph <$> arbitrary

instance Arbitrary TypeScheme where
  arbitrary = Forall <$> listOf arbitrary <*> arbitrary

-- Generators for complex data structures
genValidIdentifier :: Gen String
genValidIdentifier = do
  first <- elements ['a'..'z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

genValidTypeName :: Gen String
genValidTypeName = do
  first <- elements ['A'..'Z']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return $ first : rest

genNonEmptyString :: Gen String
genNonEmptyString = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ [' '] ++ ['_']

genPositiveInt :: Gen Int
genPositiveInt = choose (1, 1000)

genSmallInt :: Gen Int
genSmallInt = choose (0, 100)

genNonEmptyList :: Arbitrary a => Gen [a]
genNonEmptyList = listOf1 arbitrary

genUniqueStrings :: Int -> Gen [String]
genUniqueStrings n = do
  strs <- vectorOf n genValidIdentifier
  return $ nub strs

genSymbolTable :: Int -> Gen [(String, SymbolInfo)]
genSymbolTable n = do
  names <- genUniqueStrings n
  infos <- vectorOf n arbitrary
  return $ zip names infos

genTypeEnvironment :: Int -> Gen Dependencies.TypeSystem.TypeEnv
genTypeEnvironment n = do
  names <- genUniqueStrings n
  typeDefs <- vectorOf n arbitrary
  let bindings = zip names typeDefs
  return $ Dependencies.TypeSystem.TypeEnv (Map.fromList bindings) []

-- genDependencyGraph :: Int -> Gen DependencyGraph
-- genDependencyGraph n = do
--   names <- genUniqueStrings n
--   nodes <- mapM (\name -> do
--     deps <- listOf $ elements names
--     return (name, DependencyNode name deps)
--     ) names
--   return $ DependencyGraph (Map.fromList nodes)

genComplexType :: Int -> Gen Type
genComplexType depth = 
  if depth <= 0 then
    oneof
      [ TypeName <$> genValidTypeName
      , TypeName <$> genValidIdentifier  -- Using TypeName instead of TVVar
      ]
  else
    frequency
      [ (2, TypeName <$> genValidTypeName)
      , (2, TypeName <$> genValidIdentifier)  -- Using TypeName instead of TVVar
      , (1, TypeFunction <$> listOf (genComplexType (depth - 1)) <*> genComplexType (depth - 1))
      , (1, TypeRecord <$> listOf ((,) <$> genValidIdentifier <*> genComplexType (depth - 1)))
      , (1, TypeUnion <$> listOf (genComplexType (depth - 1)))
      , (1, TypeUnion <$> listOf (genComplexType (depth - 1)))
      ]

genOwnershipChain :: Int -> Gen [OwnershipType]
genOwnershipChain n = do
  names <- genUniqueStrings n
  return $ map (\name -> Owned name) names

genBorrowingScenario :: Gen (OwnershipType, [OwnershipType])
genBorrowingScenario = do
  owner <- Owned <$> genValidIdentifier
  numBorrows <- choose (0, 3)
  borrowNames <- genUniqueStrings numBorrows
  let borrows = map (\name -> elements [Borrowed name, MutBorrowed name]) borrowNames
  sequence borrows >>= \bs -> return (owner, bs)

genErrorScenario :: Gen (String, ErrorSeverity)
genErrorScenario = do
  msg <- genNonEmptyString
  severity <- arbitrary
  return (msg, severity)

genAnalysisScenario :: Gen (AnalysisPhase, AnalysisContext, [CombinedError])
genAnalysisScenario = do
  phase <- arbitrary
  context <- arbitrary
  numErrors <- choose (0, 5)
  errors <- vectorOf numErrors genErrorScenario
  return (phase, context, map (\(msg, sev) -> IntegrationError msg sev) errors)

-- Utility generators for testing edge cases
genEmptyInput :: Gen String
genEmptyInput = return ""

genWhitespaceOnly :: Gen String
genWhitespaceOnly = listOf $ elements " \t\n\r"

genSpecialChars :: Gen String
genSpecialChars = listOf $ elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"

genUnicodeString :: Gen String
genUnicodeString = listOf $ elements $ ['a'..'z'] ++ "测试内容🚀αβγ"

genLongString :: Int -> Gen String
genLongString n = listOfN n $ elements ['a'..'z']

genNestedStructure :: Int -> Gen String
genNestedStructure depth = 
  if depth <= 0 then
    return "leaf"
  else
    do
      inner <- genNestedStructure (depth - 1)
      return $ "(" ++ inner ++ ")"

genLargeInput :: Int -> Gen String
genLargeInput size = do
  lines <- vectorOf size $ genNonEmptyString
  return $ unlines lines

-- Generators for performance testing
genLargeSymbolTable :: Int -> Gen [(String, SymbolInfo)]
genLargeSymbolTable n = genSymbolTable n

genLargeTypeEnvironment :: Int -> Gen Dependencies.TypeSystem.TypeEnv
genLargeTypeEnvironment n = genTypeEnvironment n

genLargeDependencyGraph :: Int -> Gen AST
genLargeDependencyGraph n = do
  statements <- vectorOf n arbitrary
  return $ Program statements

genComplexConstraints :: Int -> Gen [TypeConstraint]
genComplexConstraints n = vectorOf n arbitrary

-- Helper functions
listOf1 :: Gen a -> Gen [a]
listOf1 gen = do
  x <- gen
  xs <- listOf gen
  return (x:xs)

listOfN :: Int -> Gen a -> Gen [a]
listOfN n gen = vectorOf n gen

-- Specialized generators for specific test scenarios
genValidTypusCode :: Gen String
genValidTypusCode = do
  directives <- listOf $ elements 
    [ "//! ownership: on"
    , "//! ownership: off"
    , "//! dependent_types: on"
    , "//! dependent_types: off"
    ]
  code <- listOf $ genNonEmptyString
  return $ unlines $ directives ++ code

genInvalidTypusCode :: Gen String
genInvalidTypusCode = do
  invalid <- listOf $ elements
    [ "invalid syntax {"
    , "unclosed string \""
    , "bad keyword xyz123"
    , "mismatched brackets [)"
    ]
  return $ unlines invalid

genMixedTypusCode :: Gen String
genMixedTypusCode = do
  valid <- genValidTypusCode
  invalid <- genInvalidTypusCode
  return $ valid ++ "\n" ++ invalid

genOwnershipCode :: Gen String
genOwnershipCode = do
  ownership <- elements
    [ "let x = String::new();"
    , "let y = x;"
    , "let z = &x;"
    , "let w = &mut x;"
    , "drop(x);"
    ]
  return $ unlines $ replicate 5 ownership

genTypeCode :: Gen String
genTypeCode = do
  types <- listOf $ elements
    [ "type Int = i32;"
    , "type String = str;"
    , "type Option<T> = Some(T) | None;"
    , "type Result<T, E> = Ok(T) | Err(E);"
    ]
  return $ unlines types