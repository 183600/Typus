{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module TestSupport.Arbitrary (
  -- Arbitrary instances
  arbitraryString,
  arbitraryChar,
  arbitraryShortString,
  arbitraryWhitespace,
  arbitraryIdentifier,
  arbitraryInt,
  arbitraryUnicodeString,
  arbitraryEscapeString,
  arbitraryStringLiteral,
  arbitraryNumericLiteral,
  arbitraryOperator,
  arbitraryAST,
  arbitraryStatement,
  arbitraryTypeVar,
  arbitraryTypeExpr,
  arbitraryTypeConstraint,
  arbitraryTypeScheme,
  arbitrarySubstitution,
  arbitraryTypeEnvironment,
  arbitrarySourcePos,
  arbitrarySourceSpan,
  arbitraryPositiveInt,
  arbitraryOwnershipError,
  arbitraryOwnershipType,
  arbitraryOwnershipTransfer,
  validTypusCode
) where

import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, frequency, choose, getPositive, arbitraryUnicodeChar, vectorOf, resize)
import qualified Data.Text as T
import qualified Data.Map as Map

import Parser
  ( FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  )
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
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
import Compiler.GoLexer
  ( GoToken(..)
  , GoTokenKind(..)
  )
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Analyzer.Types
  ( SymbolKind(..)
  , AnalysisPhase(..)
  , AnalysisContext(..)
  , AnalyzerState(..)
  )
import qualified Ownership.Common.Types as Own (OwnershipType(..), OwnershipError(..), OwnershipAnalyzer(..), OwnershipTransfer(..), newOwnershipAnalyzer)
import qualified Dependencies.TypeSystem as Dep
import Dependencies.AST (DependencyNode(..))
import qualified Dependencies.AST as Dep (AST(..), Statement(..), TypeExpr(..), Constraint(..))
import qualified Dependencies.TypeSystem as Dep (TypeConstraint(..))
import qualified Dependencies.TypeSystem as Dep (TypeScheme(..))
import qualified Compiler.TypeChecker as TC
  ( Type(..)
  , TypeEnv(..)
  , FunctionParam(..)
  , FunctionSignature(..)
  , CallExpr(..)
  , TypeError(..)
  , TypeCheckDiagnostic(..)
  )
import Compiler.ValueAnalysis (ValueInfo(..), ValueKind(..))
import qualified Compiler.ValueAnalysis as ValueAnalysis
import qualified Compiler.Errors.Core as Core (ErrorSeverity(..), ErrorLocation(..), ErrorContext(..), ErrorRecovery(..), emptyContext, TypeError(..))
import Compiler.Errors (CompilationPhase(..))

-- Helper generators
genAlphaNum :: Gen Char
genAlphaNum = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z']
  rest <- listOf genAlphaNum
  return (first : rest)

genNonEmptyString :: Gen String
genNonEmptyString = listOf genAlphaNum

genBool :: Gen Bool
genBool = elements [True, False]

genInt :: Gen Int
genInt = choose (0, 50)



-- Source location generators
-- Arbitrary instance for SourcePos is now defined in SourceLocation module


-- Arbitrary instance for SourceSpan is now defined in SourceLocation module


-- Located wrapper generator
genLocated :: Gen a -> Gen (Located a)
genLocated gen = Located <$> gen <*> arbitrary <*> arbitrary

-- Parser data type instances
instance Arbitrary FileDirectives where
  arbitrary = FileDirectives
    <$> frequency [(1, pure Nothing), (3, Just <$> genLocated genBool)]
    <*> frequency [(1, pure Nothing), (3, Just <$> genLocated genBool)]
    <*> frequency [(1, pure Nothing), (3, Just <$> genLocated genBool)]

instance Arbitrary BlockDirectives where
  arbitrary = BlockDirectives
    <$> frequency [(1, pure Nothing), (3, Just <$> genLocated genBool)]
    <*> frequency [(1, pure Nothing), (3, Just <$> genLocated genBool)]
    <*> frequency [(1, pure Nothing), (3, Just <$> genLocated genBool)]

instance Arbitrary CodeBlock where
  arbitrary = CodeBlock
    <$> arbitrary
    <*> genNonEmptyString
    <*> arbitrary

instance Arbitrary TypusFile where
  arbitrary = do
    directives <- arbitrary
    -- Memory optimization: limit buildTags to prevent excessive memory usage
    buildTags <- resize 3 $ listOf (genLocated genNonEmptyString)
    -- Memory optimization: limit blocks to prevent excessive memory usage
    blocks <- resize 5 $ listOf arbitrary
    syntaxErrors <- pure [] -- Simplified for now
    return $ TypusFile directives buildTags blocks syntaxErrors

-- Go AST generators (simplified)
instance Arbitrary ImportDecl where
  arbitrary = ImportDecl <$> frequency [(1, pure Nothing), (2, Just <$> genIdentifier)] <*> genIdentifier

instance Arbitrary FuncDecl where
  arbitrary = FuncDecl <$> listOf genNonEmptyString

instance Arbitrary TypeDecl where
  arbitrary = TypeDecl <$> listOf genIdentifier <*> arbitrary

instance Arbitrary VarDecl where
  arbitrary = VarDecl <$> listOf genIdentifier <*> arbitrary

instance Arbitrary ConstDecl where
  arbitrary = ConstDecl <$> listOf genIdentifier <*> arbitrary

instance Arbitrary PackageDecl where
  arbitrary = PackageDecl <$> genIdentifier

instance Arbitrary GoDecl where
  arbitrary = oneof
    [ GoFunc <$> arbitrary
    , GoType <$> arbitrary
    , GoVar <$> arbitrary
    , GoConst <$> arbitrary
    ]

instance Arbitrary GoTokenKind where
  arbitrary = elements
    [ TokIdentifier
    , TokKeyword
    , TokNumber
    , TokString
    , TokComment
    , TokOperator
    , TokSymbol
    , TokWhitespace
    , TokOther
    ]

instance Arbitrary GoToken where
  arbitrary = GoToken <$> arbitrary <*> genNonEmptyString

instance Arbitrary GoModule where
  arbitrary = GoModule
    -- Memory optimization: limit module dependencies to prevent excessive memory usage
    <$> resize 3 (listOf genIdentifier)
    <*> frequency [(1, pure Nothing), (2, Just <$> (PackageDecl <$> genIdentifier))]
    -- Memory optimization: limit declarations and imports to prevent excessive memory usage
    <*> resize 5 (listOf arbitrary)
    <*> resize 5 (listOf arbitrary)

-- IR generators
instance Arbitrary SourceIR where
  arbitrary = SourceIR <$> arbitrary <*> genNonEmptyString

instance Arbitrary SemanticIR where
  arbitrary = SemanticIR <$> arbitrary <*> arbitrary <*> pure []

instance Arbitrary GoIR where
  arbitrary = GoIR <$> arbitrary <*> genNonEmptyString

-- Symbol table generators (basic instances - extended ones are in ExtendedArbitrary.hs)
instance Arbitrary SymbolKind where
  arbitrary = elements
    [ SymbolVariable
    , SymbolFunction
    , SymbolType
    , SymbolConstant
    , SymbolPackage
    , SymbolModule
    ]

-- Dependencies type generators
-- TypeVar instance moved to ExtendedArbitrary to avoid conflicts

instance Arbitrary DependencyNode where
  arbitrary = do
    name <- elements ["module", "function", "type", "variable"]
    numDeps <- choose (0, 5)
    deps <- vectorOf numDeps (elements ["module", "function", "type", "variable"])
    return $ DependencyNode name deps

-- Value system generators
instance Arbitrary ValueKind where
  arbitrary = elements [ValueCopy, Reference, ValueAnalysis.Unknown]

instance Arbitrary ValueInfo where
  arbitrary = ValueInfo <$> genIdentifier <*> arbitrary <*> genInt

-- Type system generators (basic instances - extended ones are in ExtendedArbitrary)
-- Note: Extended instances for TC.Type, SymbolInfo, etc. are in ExtendedArbitrary.hs

-- ErrorSeverity instance moved to ExtendedArbitrary to avoid conflicts

-- ErrorLocation instance moved to ExtendedArbitrary to avoid conflicts

-- OwnershipError instance moved to ExtendedArbitrary to avoid conflicts

-- DependentTypeError instance moved to ExtendedArbitrary to avoid conflicts

-- These instances require TC.Type which is now defined in ExtendedArbitrary


-- Analyzer types generators
instance Arbitrary AnalysisPhase where
  arbitrary = elements [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]

instance Arbitrary AnalysisContext where
  arbitrary = AnalysisContext <$> genBool <*> genBool <*> genIdentifier <*> arbitrary

-- Ownership instances
instance Arbitrary Own.OwnershipType where
  arbitrary = oneof
    [ Own.Owned <$> genIdentifier
    , Own.Borrowed <$> genIdentifier  
    , Own.MutBorrowed <$> genIdentifier
    ]







instance Arbitrary AnalyzerState where
  arbitrary = AnalyzerState
    <$> pure Own.newOwnershipAnalyzer -- OwnershipAnalyzer
    <*> pure Dep.newDependentTypeChecker -- DependentTypeChecker
    <*> genInt
    <*> pure mempty -- SymbolTable
    <*> arbitrary -- AnalysisContext
    <*> pure [] -- CombinedError
    <*> pure [] -- OwnershipErrors
    <*> pure [] -- DependentTypeErrors

instance Arbitrary CompilationPhase where
  arbitrary = elements 
    [ LexingPhase
    , ParsingPhase
    , TypeCheckingPhase
    , OwnershipAnalysisPhase
    , DependentTypeCheckingPhase
    , CodeGenerationPhase
    , OptimizationPhase
    ]

-- ErrorRecovery and ErrorCategory instances moved to ExtendedArbitrary to avoid conflicts

-- CompilerError instance requires Core.ErrorSeverity and Core.ErrorLocation which are now in ExtendedArbitrary
-- instance Arbitrary CompilerError where
--   arbitrary = do
--     errorId <- genIdentifier
--     severity <- arbitrary
--     category <- arbitrary
--     message <- T.pack <$> genNonEmptyString
--     location <- arbitrary
--     context <- pure Core.emptyContext
--     recovery <- arbitrary
--     suggestions <- listOf (T.pack <$> genNonEmptyString)
--     relatedErrors <- pure []
--     errorChain <- pure []
--     timestamp <- pure Nothing
    
--     let typeError = Core.TypeError
--           { Core.errorId = errorId
--           , Core.severity = severity
--           , Core.category = category
--           , Core.message = message
--           , Core.location = location
--           , Core.context = context
--           , Core.recovery = recovery
--           , Core.suggestions = suggestions
--           , Core.relatedErrors = relatedErrors
--           , Core.errorChain = errorChain
--           , Core.timestamp = timestamp
--           }
    
--     sourceContext <- frequency [(1, pure Nothing), (2, Just <$> genNonEmptyString)]
--     stackTrace <- listOf genIdentifier
--     phase <- arbitrary
    
--     return $ CompilerError
--       { ceError = typeError
--       , ceSourceContext = sourceContext
--       , ceStackTrace = stackTrace
--       , cePhase = phase
--       }

-- String generators for testing edge cases






-- Property test helpers
class WellFormed a where
  isWellFormed :: a -> Bool

instance WellFormed ImportDecl where
  isWellFormed (ImportDecl _ path) = not (null path)

instance WellFormed GoDecl where
  isWellFormed _ = True  -- Simplified implementation

instance WellFormed GoModule where
  isWellFormed (GoModule _ _pkg imports decls) = 
    all isWellFormed imports && all isWellFormed decls
  -- Simplified implementation

-- Generators for well-formed values




-- | Generator for arbitrary strings
arbitraryString :: Gen String
arbitraryString = do
  size <- choose (0, 20)
  vectorOf size arbitrary

-- | Generator for arbitrary characters
arbitraryChar :: Gen Char
arbitraryChar = arbitrary

-- | Generator for arbitrary positive integers
arbitraryPositiveInt :: Gen Int
arbitraryPositiveInt = getPositive <$> arbitrary

-- | Generator for arbitrary SourcePos
arbitrarySourcePos :: Gen SourcePos
arbitrarySourcePos = SourcePos <$> arbitraryPositiveInt <*> arbitraryPositiveInt <*> arbitrary

-- | Generator for arbitrary SourceSpan
arbitrarySourceSpan :: Gen SourceSpan
arbitrarySourceSpan = SourceSpan <$> arbitrarySourcePos <*> arbitrarySourcePos



arbitraryInt :: Gen Int
arbitraryInt = arbitrary

-- | Generator for arbitrary identifiers
arbitraryIdentifier :: Gen String
arbitraryIdentifier = do
  firstChar <- elements ['a'..'z']
  restChars <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['_']
  return $ firstChar : restChars

-- | Generator for arbitrary unicode strings
arbitraryUnicodeString :: Gen String
arbitraryUnicodeString = do
  size <- choose (0, 15)
  vectorOf size arbitraryUnicodeChar

-- | Generator for arbitrary whitespace strings
arbitraryWhitespace :: Gen String
arbitraryWhitespace = listOf $ elements " \t\n\r"

-- | Generator for arbitrary short strings
arbitraryShortString :: Gen String
arbitraryShortString = do
  size <- choose (1, 10)
  vectorOf size genAlphaNum

-- | Generator for valid Typus code
validTypusCode :: Gen String
validTypusCode = oneof
  [ pure ""
  , pure "test {}"
  , do
      ident <- genIdentifier
      content <- genNonEmptyString
      return $ ident ++ " {\n" ++ content ++ "\n}"
  , do
      ident1 <- genIdentifier
      ident2 <- genIdentifier
      content1 <- genNonEmptyString
      content2 <- genNonEmptyString
      return $ ident1 ++ " {\n" ++ content1 ++ "\n" ++ ident2 ++ " {\n" ++ content2 ++ "\n}\n}"
  ]

-- | Generator for arbitrary escape strings
arbitraryEscapeString :: Gen String
arbitraryEscapeString = oneof
  [ pure "\\n"
  , pure "\\t"
  , pure "\\r"
  , pure "\\\\"
  , pure "\\\""
  , pure "\\'"
  , do
      base <- genNonEmptyString
      return $ "\\" ++ base
  ]

-- | Generator for arbitrary string literals
arbitraryStringLiteral :: Gen String
arbitraryStringLiteral = oneof
  [ do
      content <- genNonEmptyString
      return $ "\"" ++ content ++ "\""
  , do
      content <- arbitraryEscapeString
      return $ "\"" ++ content ++ "\""
  , pure "\"\""
  ]

-- | Generator for arbitrary numeric literals
arbitraryNumericLiteral :: Gen String
arbitraryNumericLiteral = oneof
  [ do
      num <- choose (0 :: Int, 1000)
      return $ show (num :: Int)
  , do
      num <- choose (0.0 :: Double, 1000.0)
      return $ show (num :: Double)
  , pure "0"
  , pure "1"
  , pure "42"
  , pure "3.14"
  ]

-- | Generator for arbitrary operators
arbitraryOperator :: Gen String
arbitraryOperator = elements
  [ "+", "-", "*", "/", "%", "++", "--"
  , "==", "!=", "<", ">", "<=", ">="
  , "&&", "||", "!", "&", "|", "^", "~"
  , "<<", ">>", "&&=", "||=", "&=", "|=", "^="
  , "+=", "-=", "*=", "/=", "%="
  , "=", "+=", "-=", "*=", "/=", "%="
  , "->", "<-", "::", "..", "..."
  ]

-- | Generator for arbitrary ownership errors
arbitraryOwnershipError :: Gen Own.OwnershipError
arbitraryOwnershipError = oneof
  [ Own.UseAfterMove <$> genIdentifier
  , Own.DoubleMove <$> genIdentifier <*> genIdentifier
  , Own.BorrowWhileMoved <$> genIdentifier
  , Own.MutBorrowWhileBorrowed <$> genIdentifier
  , Own.BorrowWhileMutBorrowed <$> genIdentifier
  , Own.MultipleMutBorrows <$> genIdentifier
  , Own.UseWhileMutBorrowed <$> genIdentifier
  , Own.OutOfScope <$> genIdentifier
  , Own.BorrowError <$> genNonEmptyString
  , Own.ParseError <$> genNonEmptyString
  , Own.CrossFunctionMove <$> genIdentifier <*> genIdentifier
  , Own.ParameterMoveMismatch <$> genIdentifier
  , Own.ControlFlowError <$> genNonEmptyString
  , Own.PathSensitiveError <$> genNonEmptyString
  , Own.LoopOwnershipError <$> genNonEmptyString
  ]

-- | Generator for arbitrary ownership types
arbitraryOwnershipType :: Gen Own.OwnershipType
arbitraryOwnershipType = arbitrary

-- | Generator for arbitrary ownership transfers
arbitraryOwnershipTransfer :: Gen Own.OwnershipTransfer
arbitraryOwnershipTransfer = Own.OwnershipTransfer <$> genIdentifier <*> genIdentifier

-- | Generator for arbitrary type variables
arbitraryTypeVar :: Gen Dep.TypeVar
arbitraryTypeVar = oneof
  [ Dep.TVCon <$> genIdentifier
  , Dep.TVVar <$> genIdentifier
  -- Memory optimization: limit recursive type construction to prevent excessive memory usage
  , Dep.TVApp <$> genIdentifier <*> resize 2 (listOf arbitraryTypeVar)
  , Dep.TVFun <$> resize 2 (listOf arbitraryTypeVar) <*> arbitraryTypeVar
  , Dep.TVTuple <$> resize 3 (listOf arbitraryTypeVar)
  ]

-- | Generator for arbitrary constraints
arbitraryConstraint :: Gen Dep.Constraint
arbitraryConstraint = Dep.SizeGT <$> (T.pack <$> genIdentifier) <*> arbitrary

-- | Generator for arbitrary type expressions
arbitraryTypeExpr :: Gen Dep.TypeExpr
arbitraryTypeExpr = oneof
  [ Dep.SimpleT <$> (T.pack <$> genIdentifier)
  -- Memory optimization: limit generic type parameters to prevent excessive memory usage
  , Dep.GenericT <$> (T.pack <$> genIdentifier) <*> resize 2 (listOf arbitraryTypeExpr)
  -- Memory optimization: limit function parameters and constraints to prevent excessive memory usage
  , Dep.FuncT <$> resize 3 (listOf ((,) <$> (T.pack <$> genIdentifier) <*> arbitraryTypeExpr)) <*> arbitraryTypeExpr
  , Dep.RefineT <$> arbitraryTypeExpr <*> resize 2 (listOf arbitraryConstraint)
  ]

-- | Generator for arbitrary AST
arbitraryAST :: Gen Dep.AST
arbitraryAST = do
  -- Memory optimization: limit number of statements to prevent excessive memory usage
  statements <- resize 5 $ listOf arbitraryStatement
  return $ Dep.Program statements

-- | Generator for arbitrary statements
arbitraryStatement :: Gen Dep.Statement
arbitraryStatement = oneof
  [ Dep.STypeDef <$> (T.pack <$> genIdentifier) <*> (map T.pack <$> listOf genIdentifier) <*> pure []
  , Dep.STypeAlias <$> (T.pack <$> genIdentifier) <*> (Dep.SimpleT <$> (T.pack <$> genIdentifier)) <*> pure []
  ]

-- | Generator for arbitrary type environment
arbitraryTypeEnvironment :: Gen [String]
arbitraryTypeEnvironment = listOf genIdentifier

-- | Generator for arbitrary type constraints
arbitraryTypeConstraint :: Gen Dep.TypeConstraint
arbitraryTypeConstraint = oneof
  [ Dep.Equal <$> arbitraryTypeVar <*> arbitraryTypeVar
  , Dep.Subtype <$> arbitraryTypeVar <*> arbitraryTypeVar
  -- Memory optimization: limit predicate parameters to prevent excessive memory usage
  , Dep.Predicate <$> genIdentifier <*> resize 3 (listOf arbitraryTypeVar)
  , Dep.TypeSizeGE <$> arbitraryTypeVar <*> arbitrary
  , Dep.TypeSizeGT <$> arbitraryTypeVar <*> arbitrary
  , Dep.TypeRange <$> arbitraryTypeVar <*> arbitrary <*> arbitrary
  ]

-- | Generator for arbitrary type schemes
arbitraryTypeScheme :: Gen Dep.TypeScheme
arbitraryTypeScheme = do
  vars <- listOf genIdentifier
  typ <- arbitraryTypeVar
  return $ Dep.Forall vars typ

-- | Generator for arbitrary substitutions
arbitrarySubstitution :: Gen Dep.Substitution
arbitrarySubstitution = do
  size <- choose (0, 10)
  keys <- vectorOf size genIdentifier
  values <- vectorOf size arbitraryTypeVar
  return $ Map.fromList (zip keys values)
