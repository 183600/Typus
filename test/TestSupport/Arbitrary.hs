{-# LANGUAGE CPP #-}

-- | QuickCheck Arbitrary instances for Typus data types
module TestSupport.Arbitrary where

import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, sized, frequency, choose)
import qualified Data.Text as T
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isAlphaNum, isSpace)

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
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import Analyzer.Types
  ( SymbolInfo(..)
  , SymbolKind(..)
  , AnalysisResult(..)
  , AnalysisPhase(..)
  , AnalysisContext(..)
  , AnalyzerState(..)
  , CombinedError(..)
  )
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
import Ownership (OwnershipType(..), OwnershipError(..))
import Compiler.Errors.Core (ErrorSeverity(..), ErrorLocation(..), ErrorContext(..), ErrorRecovery(..), emptyContext, TypeError(..))
import Compiler.Errors (CompilerError(..), CompilationPhase(..))
import qualified Compiler.Errors.Core as Core
import qualified Compiler.ValueAnalysis as ValueAnalysis
import qualified Dependencies as Dep

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
genInt = choose (0, 100)

genSmallInt :: Gen Int
genSmallInt = choose (0, 10)

-- Source location generators
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> genInt <*> genInt <*> genInt

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary

-- Located wrapper generator
genLocated :: Gen a -> Gen (Located a)
genLocated gen = Located <$> arbitrary <*> gen

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
    buildTags <- listOf (genLocated genNonEmptyString)
    blocks <- listOf arbitrary
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

instance Arbitrary GoModule where
  arbitrary = GoModule
    <$> listOf genIdentifier
    <*> frequency [(1, pure Nothing), (2, Just <$> (PackageDecl <$> genIdentifier))]
    <*> listOf arbitrary
    <*> listOf arbitrary

-- IR generators
instance Arbitrary SourceIR where
  arbitrary = SourceIR <$> arbitrary <*> genNonEmptyString

instance Arbitrary SemanticIR where
  arbitrary = SemanticIR <$> arbitrary <*> arbitrary <*> pure []

instance Arbitrary GoIR where
  arbitrary = GoIR <$> arbitrary <*> genNonEmptyString

-- Symbol table generators
instance Arbitrary SymbolKind where
  arbitrary = elements
    [ SymbolVariable
    , SymbolFunction
    , SymbolType
    , SymbolConstant
    , SymbolPackage
    , SymbolModule
    ]

instance Arbitrary SymbolInfo where
  arbitrary = SymbolInfo
    <$> arbitrary
    <*> frequency [(1, pure Nothing), (2, Just <$> (Dep.TVCon <$> genIdentifier))]
    <*> frequency [(1, pure Nothing), (2, Just <$> arbitrary)]
    <*> genInt
    <*> genBool
    <*> genBool
    <*> pure []

-- Dependencies type generators
instance Arbitrary Dep.TypeVar where
  arbitrary = oneof
    [ Dep.TVCon <$> genIdentifier
    , Dep.TVVar <$> genIdentifier
    , Dep.TVApp <$> genIdentifier <*> listOf arbitrary
    , Dep.TVFun <$> listOf arbitrary <*> arbitrary
    , Dep.TVTuple <$> listOf arbitrary
    ]

-- Value system generators
instance Arbitrary ValueKind where
  arbitrary = elements [ValueCopy, Reference, ValueAnalysis.Unknown]

instance Arbitrary ValueInfo where
  arbitrary = ValueInfo <$> genIdentifier <*> arbitrary <*> genInt

-- Type system generators
instance Arbitrary TC.Type where
  arbitrary = oneof
    [ TC.TypeName <$> genIdentifier
    , pure TC.UnknownType
    ]

instance Arbitrary TC.FunctionParam where
  arbitrary = TC.FunctionParam <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TC.FunctionSignature where
  arbitrary = TC.FunctionSignature <$> arbitrary <*> arbitrary

instance Arbitrary TC.CallExpr where
  arbitrary = TC.CallExpr <$> genIdentifier <*> arbitrary

instance Arbitrary TC.TypeError where
  arbitrary = TC.TypeError <$> arbitrary <*> genIdentifier

instance Arbitrary TC.TypeCheckDiagnostic where
  arbitrary = TC.TypeCheckDiagnostic <$> arbitrary <*> genIdentifier

instance Arbitrary TC.TypeEnv where
  arbitrary = TC.TypeEnv <$> arbitrary <*> arbitrary

-- Error severity generators
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Error, Warning, Info]

-- Ownership types generators
instance Arbitrary OwnershipType where
  arbitrary = elements [Owned "test", Borrowed "test", MutBorrowed "test"]

instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> genIdentifier
    , DoubleMove <$> genIdentifier <*> genIdentifier
    , BorrowWhileMoved <$> genIdentifier
    , MutBorrowWhileBorrowed <$> genIdentifier
    , BorrowWhileMutBorrowed <$> genIdentifier
    , MultipleMutBorrows <$> genIdentifier
    , UseWhileMutBorrowed <$> genIdentifier
    , OutOfScope <$> genIdentifier
    , BorrowError <$> genIdentifier
    , ParseError <$> genIdentifier
    , CrossFunctionMove <$> genIdentifier <*> genIdentifier
    , ParameterMoveMismatch <$> genIdentifier
    , ControlFlowError <$> genIdentifier
    , PathSensitiveError <$> genIdentifier
    , LoopOwnershipError <$> genIdentifier
    ]

instance Arbitrary Dep.TypeConstraint where
  arbitrary = oneof
    [ Dep.Equal <$> arbitrary <*> arbitrary
    , Dep.Subtype <$> arbitrary <*> arbitrary
    , Dep.Predicate <$> genIdentifier <*> listOf arbitrary
    , Dep.TypeSizeGE <$> arbitrary <*> genInt
    , Dep.TypeSizeGT <$> arbitrary <*> genInt
    , Dep.TypeRange <$> arbitrary <*> genInt <*> genInt
    ]

instance Arbitrary Dep.DependentTypeError where
  arbitrary = oneof
    [ Dep.DependentTypeMismatch <$> arbitrary <*> arbitrary
    , Dep.ConstraintViolation <$> genIdentifier <*> arbitrary
    , Dep.TypeNotFound <$> genIdentifier
    , Dep.InvalidTypeArgument <$> genIdentifier
    , Dep.UnsolvableConstraint <$> arbitrary
    , Dep.DependentInfiniteType <$> genIdentifier <*> arbitrary
    , Dep.AmbiguousType <$> genIdentifier
    , Dep.ParseError <$> genIdentifier
    , Dep.SemanticError <$> genIdentifier
    ]

-- Analyzer types generators
instance Arbitrary AnalysisPhase where
  arbitrary = elements [InitialPhase, OwnershipPhase, DependentTypePhase, IntegrationPhase]

instance Arbitrary AnalysisContext where
  arbitrary = AnalysisContext <$> genBool <*> genBool <*> genIdentifier <*> arbitrary

instance Arbitrary CombinedError where
  arbitrary = oneof
    [ OwnershipErrorCombined <$> arbitrary <*> arbitrary
    , DependentTypeErrorCombined <$> arbitrary <*> arbitrary
    , IntegrationError <$> genIdentifier <*> arbitrary
    , CrossAnalyzerError <$> genIdentifier <*> arbitrary <*> listOf arbitrary
    ]

instance Arbitrary AnalysisResult where
  arbitrary = AnalysisResult
    <$> listOf ((,) <$> arbitrary <*> arbitrary)
    <*> listOf ((,) <$> arbitrary <*> arbitrary)
    <*> listOf arbitrary
    <*> listOf genIdentifier
    <*> listOf genIdentifier
    <*> pure mempty

instance Arbitrary AnalyzerState where
  arbitrary = AnalyzerState
    <$> pure undefined -- OwnershipAnalyzer
    <*> pure undefined -- DependentTypeChecker
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

instance Arbitrary Core.ErrorRecovery where
  arbitrary = frequency
    [ (1, pure Core.fatalRecovery)
    , (2, pure Core.errorRecovery)
    , (3, pure Core.warningRecovery)
    , (4, pure Core.infoRecovery)
    ]

instance Arbitrary Core.ErrorLocation where
  arbitrary = Core.ErrorLocation
    <$> frequency [(1, pure Nothing), (3, Just <$> genIdentifier)]
    <*> genInt
    <*> genInt
    <*> frequency [(1, pure Nothing), (2, Just <$> genInt)]
    <*> frequency [(1, pure Nothing), (2, Just <$> genInt)]

instance Arbitrary Core.ErrorCategory where
  arbitrary = elements
    [ Core.TypeChecking
    , Core.Ownership
    , Core.Parsing
    , Core.Semantic
    , Core.Runtime
    , Core.Constraint
    , Core.Inference
    , Core.Integration
    , Core.Unknown
    ]

instance Arbitrary CompilerError where
  arbitrary = do
    errorId <- genIdentifier
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> genNonEmptyString
    location <- arbitrary
    context <- pure emptyContext
    recovery <- arbitrary
    suggestions <- listOf (T.pack <$> genNonEmptyString)
    relatedErrors <- pure []
    errorChain <- pure []
    timestamp <- pure Nothing
    
    let typeError = Core.TypeError
          { Core.errorId = errorId
          , Core.severity = severity
          , Core.category = category
          , Core.message = message
          , Core.location = location
          , Core.context = context
          , Core.recovery = recovery
          , Core.suggestions = suggestions
          , Core.relatedErrors = relatedErrors
          , Core.errorChain = errorChain
          , Core.timestamp = timestamp
          }
    
    sourceContext <- frequency [(1, pure Nothing), (2, Just <$> genNonEmptyString)]
    stackTrace <- listOf genIdentifier
    phase <- arbitrary
    
    return $ CompilerError
      { ceError = typeError
      , ceSourceContext = sourceContext
      , ceStackTrace = stackTrace
      , cePhase = phase
      }

-- String generators for testing edge cases
genValidIdentifier :: Gen String
genValidIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return (first : rest)

genValidDirective :: Gen String
genValidDirective = oneof
  [ pure "//! ownership: on"
  , pure "//! ownership: off"
  , pure "//! dependent_types: on"
  , pure "//! dependent_types: off"
  , pure "//! constraints: on"
  , pure "//! constraints: off"
  , do
      own <- elements ["on", "off"]
      dep <- elements ["on", "off"]
      return $ "//! ownership: " ++ own ++ ", dependent_types: " ++ dep
  ]

genValidGoCode :: Gen String
genValidGoCode = do
  lines <- listOf $ oneof
    [ pure "package main"
    , pure "import \"fmt\""
    , pure "func main() {"
    , pure "fmt.Println(\"Hello, World!\")"
    , pure "}"
    , genIdentifier >>= \ident -> return $ "var " ++ ident ++ " int"
    , genIdentifier >>= \ident -> return $ "func " ++ ident ++ "() {}"
    ]
  return $ unlines lines

-- Property test helpers
class WellFormed a where
  isWellFormed :: a -> Bool

instance WellFormed ImportDecl where
  isWellFormed (ImportDecl _ path) = not (null path)

instance WellFormed GoDecl where
  isWellFormed _ = True  -- Simplified implementation

instance WellFormed GoModule where
  isWellFormed (GoModule _ pkg imports decls) = 
    all isWellFormed imports && all isWellFormed decls
  -- Simplified implementation

-- Generators for well-formed values
genWellFormedImportDecl :: Gen ImportDecl
genWellFormedImportDecl = ImportDecl <$> frequency [(1, pure Nothing), (2, Just <$> genIdentifier)] <*> genIdentifier

genWellFormedGoModule :: Gen GoModule
genWellFormedGoModule = GoModule
  <$> listOf genIdentifier
  <*> frequency [(1, pure Nothing), (2, Just <$> (PackageDecl <$> genIdentifier))]
  <*> listOf genWellFormedImportDecl
  <*> listOf arbitrary