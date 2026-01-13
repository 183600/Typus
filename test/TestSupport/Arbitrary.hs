{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module TestSupport.Arbitrary where

import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, frequency, choose, getPositive, arbitraryUnicodeChar, vectorOf)
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
import Compiler.Errors.Core (ErrorSeverity(..), ErrorLocation(..), ErrorContext(..), ErrorRecovery(..), emptyContext, TypeError(..))
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
genInt = choose (0, 100)

genSmallInt :: Gen Int
genSmallInt = choose (0, 10)

-- Source location generators
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> choose (1, 100) <*> choose (1, 100) <*> choose (0, 10000)

instance Arbitrary SourceSpan where
  arbitrary = do
    startLine <- choose (1, 100)
    startCol <- choose (1, 100)
    startOffset <- choose (0, 10000)
    let startPos = SourcePos startLine startCol startOffset
    
    endLine' <- choose (startLine, startLine + 10)  -- End line >= start line
    endCol <- if endLine' == startLine 
              then choose (startCol, startCol + 50)  -- Same line: end column >= start column
              else choose (1, 100)  -- Different line: any column
    endOffset <- choose (startOffset, startOffset + 1000)
    let endPos = SourcePos endLine' endCol endOffset
    
    return $ SourceSpan startPos endPos

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

-- CompilerError instance requires ErrorSeverity and ErrorLocation which are now in ExtendedArbitrary
-- instance Arbitrary CompilerError where
--   arbitrary = do
--     errorId <- genIdentifier
--     severity <- arbitrary
--     category <- arbitrary
--     message <- T.pack <$> genNonEmptyString
--     location <- arbitrary
--     context <- pure emptyContext
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
  codeLines <- listOf $ oneof
    [ pure "package main"
    , pure "import \"fmt\""
    , pure "func main() {"
    , pure "fmt.Println(\"Hello, World!\")"
    , pure "}"
    , genIdentifier >>= \ident -> return $ "var " ++ ident ++ " int"
    , genIdentifier >>= \ident -> return $ "func " ++ ident ++ "() {}"
    ]
  return $ unlines codeLines

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
genWellFormedImportDecl :: Gen ImportDecl
genWellFormedImportDecl = ImportDecl <$> frequency [(1, pure Nothing), (2, Just <$> genIdentifier)] <*> genIdentifier

genWellFormedGoModule :: Gen GoModule
genWellFormedGoModule = GoModule
  <$> listOf genIdentifier
  <*> frequency [(1, pure Nothing), (2, Just <$> (PackageDecl <$> genIdentifier))]
  <*> listOf genWellFormedImportDecl
  <*> listOf arbitrary

-- | Generator for arbitrary strings
arbitraryString :: Gen String
arbitraryString = listOf arbitrary

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

-- | Accessor functions for SourcePos
spLine :: SourcePos -> Int
spLine = posLine

spColumn :: SourcePos -> Int
spColumn = posColumn

-- | Accessor functions for SourceSpan
ssStart :: SourceSpan -> SourcePos
ssStart = SourceLocation.spanStart

ssEnd :: SourceSpan -> SourcePos
ssEnd = SourceLocation.spanEnd

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
arbitraryUnicodeString = listOf arbitraryUnicodeChar

-- | Generator for arbitrary whitespace strings
arbitraryWhitespace :: Gen String
arbitraryWhitespace = listOf $ elements " \t\n\r"

-- | Generator for arbitrary short strings
arbitraryShortString :: Gen String
arbitraryShortString = genNonEmptyString

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
  , Dep.TVApp <$> genIdentifier <*> listOf arbitraryTypeVar
  , Dep.TVFun <$> listOf arbitraryTypeVar <*> arbitraryTypeVar
  , Dep.TVTuple <$> listOf arbitraryTypeVar
  ]

-- | Generator for arbitrary constraints
arbitraryConstraint :: Gen Dep.Constraint
arbitraryConstraint = Dep.SizeGT <$> (T.pack <$> genIdentifier) <*> arbitrary

-- | Generator for arbitrary type expressions
arbitraryTypeExpr :: Gen Dep.TypeExpr
arbitraryTypeExpr = oneof
  [ Dep.SimpleT <$> (T.pack <$> genIdentifier)
  , Dep.GenericT <$> (T.pack <$> genIdentifier) <*> listOf arbitraryTypeExpr
  , Dep.FuncT <$> (listOf ((,) <$> (T.pack <$> genIdentifier) <*> arbitraryTypeExpr)) <*> arbitraryTypeExpr
  , Dep.RefineT <$> arbitraryTypeExpr <*> (listOf arbitraryConstraint)
  ]

-- | Generator for arbitrary AST
arbitraryAST :: Gen Dep.AST
arbitraryAST = do
  statements <- listOf arbitraryStatement
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
  , Dep.Predicate <$> genIdentifier <*> listOf arbitraryTypeVar
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
