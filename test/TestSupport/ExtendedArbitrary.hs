{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TestSupport.ExtendedArbitrary
  ( genUniqueIdentifier
  , genCamelCaseIdentifier
  , genSnakeCaseIdentifier
  , genPascalCaseIdentifier
  , genGoTypeName
  , genGoVarName
  , genGoPackageName
  , genGoImportPath
  , genIdentifier
  , genBool
  , genNonEmptyString
  , genLocated
  , genGoFunctionDecl
  , genGoVarDecl
  , genGoTypeDecl
  , genGoCodeSnippet
  , genTypeError
  , WellFormedExtended(..)
  , genWellFormedExtendedGoModule
  , genWellFormedExtendedImportDecl
  , genWellFormedExtendedType
  , genNonEmptyList
  , genSortedIntList
  , genUniqueStringList
  , genValidGoCode
  , prop_roundtrip_consistency
  , prop_idempotent
  , prop_associative
  , prop_commutative
  ) where

import Test.QuickCheck (Arbitrary(..), Gen, oneof, elements, listOf, sized, frequency, choose, suchThat, Property, resize, listOf1, vectorOf, property, (===))
import qualified Data.Text as T
import Data.List (nub, sort, intersperse, isPrefixOf)
import Data.Char (toLower, toUpper)
import TestSupport.Arbitrary ()

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
import Compiler.IR (SourceIR(..))
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
  , TypeConstraint(..)
  )
import Ownership (OwnershipType(..), OwnershipError(..))
import qualified Ownership.Common.Types as Own (OwnershipAnalyzer(..))
import Compiler.Errors.Core (ErrorSeverity(..))
import Compiler.Errors (CompilerError(..), CompilationPhase(..))
import qualified Compiler.Errors.Core as Core
import qualified Dependencies as Dep
import qualified Dependencies.TypeSystem as DepT (TypeEnv(..), TypeDef(..), DependentTypeChecker(..))
import qualified Dependencies.AST as DepAST (DependencyGraph(..), DependencyNode(..))
import SyntaxValidator (SyntaxError(..), ErrorType(..))
import qualified Ownership.Parser as Own
  ( Expr(..)
  , Stmt(..)
  , AssignOp(..)
  , UnaryOp(..)
  , Directive(..)
  )
import qualified Ownership.Common.Lexer as OwnL (Pos(..), Token(..))


-- Arbitrary instances for Ownership Parser types
instance Arbitrary Own.AssignOp where
  arbitrary = elements [Own.OpAssign, Own.OpWalrus]

instance Arbitrary Own.UnaryOp where
  arbitrary = elements [Own.UBorrow, Own.UMutBorrow]

instance Arbitrary Own.Directive where
  arbitrary = Own.Directive <$> arbitrary

instance Arbitrary OwnL.Pos where
  arbitrary = OwnL.Pos <$> choose (1, 100) <*> choose (1, 100)



instance Arbitrary Own.Expr where
  arbitrary = sized $ \n -> if n <= 0 then
    Own.ELitNum <$> genIdentifier <*> arbitrary
  else oneof
    [ Own.EIdent <$> genIdentifier <*> arbitrary
    , Own.ECall <$> genIdentifier <*> listOf (resize (n-1) arbitrary) <*> arbitrary
    , Own.EUnary <$> arbitrary <*> resize (n-1) arbitrary <*> arbitrary
    , Own.ELitStr <$> genIdentifier <*> arbitrary
    , Own.ELitNum <$> genIdentifier <*> arbitrary
    , Own.EUnknown <$> (pure []) <*> arbitrary  -- Simplified: empty token list
    ]

instance Arbitrary Own.Stmt where
  arbitrary = sized $ \n -> if n <= 0 then
    Own.SExpr <$> arbitrary <*> arbitrary
  else oneof
    [ Own.SVarDecl <$> genIdentifier <*> frequency [(1, pure Nothing), (2, Just <$> arbitrary)] <*> arbitrary
    , Own.SLetDecl <$> genIdentifier <*> frequency [(1, pure Nothing), (2, Just <$> arbitrary)] <*> arbitrary
    , Own.SAssignStmt <$> genIdentifier <*> arbitrary <*> resize (n-1) arbitrary <*> arbitrary
    , Own.SExpr <$> arbitrary <*> arbitrary
    , Own.SBlock <$> listOf (resize (n-1) arbitrary) <*> arbitrary
    , Own.SFunc <$> listOf (resize (n-1) arbitrary) <*> arbitrary
    , Own.SFor <$> listOf (resize (n-1) arbitrary) <*> arbitrary
    , Own.SDirectiveLine <$> arbitrary <*> arbitrary
    , Own.SDirectiveBlock <$> arbitrary <*> listOf (resize (n-1) arbitrary) <*> arbitrary
    ]

-- Arbitrary instances for SyntaxValidator types
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
    , SyntaxWarning
    ]

instance Arbitrary SyntaxError where
  arbitrary = SyntaxError
    <$> arbitrary
    <*> genNonEmptyString
    <*> choose (1, 1000)
    <*> choose (1, 1000)
    <*> genNonEmptyString

-- =================================================================
-- Extended Arbitrary instances for more comprehensive testing
-- =================================================================

-- Generate unique identifiers to avoid naming conflicts
genUniqueIdentifier :: Gen String
genUniqueIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z']
  middle <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  suffix <- choose (1000, 9999 :: Int)
  return $ first : middle ++ show suffix

-- Generate identifiers with specific patterns
genCamelCaseIdentifier :: Gen String
genCamelCaseIdentifier = do
  wordList <- listOf $ do
    first <- elements $ ['a'..'z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
    return $ first : rest
  return $ concat $ zipWith (\(i :: Int) w -> case w of
    [] -> ""
    (c:cs) -> if i == 0 then w else toUpper c : cs) [0..] wordList

genSnakeCaseIdentifier :: Gen String
genSnakeCaseIdentifier = do
  wordList <- listOf $ do
    chars <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
    return chars
  return $ concat $ intersperse "_" wordList

genPascalCaseIdentifier :: Gen String
genPascalCaseIdentifier = do
  wordList <- listOf $ do
    first <- elements $ ['a'..'z']
    rest <- listOf $ elements $ ['a'..'z'] ++ ['0'..'9']
    return $ first : rest
  return $ concat $ map (\w -> case w of
    [] -> ""
    (c:cs) -> toUpper c : cs) wordList

-- Generate type names following Go conventions
genGoTypeName :: Gen String
genGoTypeName = oneof [genPascalCaseIdentifier, genCamelCaseIdentifier]

genGoVarName :: Gen String
genGoVarName = oneof [genCamelCaseIdentifier, genSnakeCaseIdentifier]

genGoPackageName :: Gen String
genGoPackageName = do
  name <- genSnakeCaseIdentifier
  return $ map toLower name

-- Generate valid Go import paths
genGoImportPath :: Gen String
genGoImportPath = do
  parts <- choose (1 :: Int, 3 :: Int)
  domain <- elements ["github.com", "gitlab.com", "golang.org", "example.com"]
  user <- genSnakeCaseIdentifier
  repo <- genSnakeCaseIdentifier
  pkg <- genSnakeCaseIdentifier
  case parts of
    1 -> return $ domain ++ "/" ++ user
    2 -> return $ domain ++ "/" ++ user ++ "/" ++ repo
    _ -> return $ domain ++ "/" ++ user ++ "/" ++ repo ++ "/" ++ pkg

-- Generate basic identifiers
genIdentifier :: Gen String
genIdentifier = genUniqueIdentifier

-- Generate boolean values
genBool :: Gen Bool
genBool = arbitrary

-- Generate non-empty strings
genNonEmptyString :: Gen String
genNonEmptyString = do
  n <- choose (1, 20)
  sequence $ replicate n $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

-- Located wrapper generator
genLocated :: Gen a -> Gen (Located a)
genLocated gen = do
  value <- gen
  pos <- arbitrary
  srcSpan <- arbitrary
  return $ Located value pos srcSpan

-- Generate realistic Go code snippets
genGoFunctionDecl :: Gen String
genGoFunctionDecl = do
  funcName <- genGoVarName
  params <- listOf $ do
    paramType <- genGoTypeName
    paramName <- genGoVarName
    return $ paramName ++ " " ++ paramType
  returnType <- frequency [(1, return ""), (2, (" " ++) <$> genGoTypeName)]
  let paramStr = if null params then "" else unwords params ++ " "
  return $ "func " ++ funcName ++ "(" ++ paramStr ++ ")" ++ returnType ++ " {"

genGoVarDecl :: Gen String
genGoVarDecl = do
  varName <- genGoVarName
  varType <- genGoTypeName
  value <- frequency [(1, return ""), (2, (" = " ++) <$> genIdentifier)]
  return $ "var " ++ varName ++ " " ++ varType ++ value

genGoTypeDecl :: Gen String
genGoTypeDecl = do
  typeName <- genGoTypeName
  typeDef <- oneof
    [ return "struct { }"
    , do
        fields <- listOf $ do
          fieldName <- genGoVarName
          fieldType <- genGoTypeName
          return $ fieldName ++ " " ++ fieldType
        return $ "struct { " ++ unlines fields ++ " }"
    , genGoTypeName
    ]
  return $ "type " ++ typeName ++ " " ++ typeDef

genGoCodeSnippet :: Gen String
genGoCodeSnippet = do
  codeLines <- listOf $ oneof
    [ pure "package main"
    , genGoImportPath >>= \path -> return $ "import \"" ++ path ++ "\""
    , genGoFunctionDecl
    , genGoVarDecl
    , genGoTypeDecl
    , pure "return nil"
    , pure "if err != nil { return err }"
    , pure "fmt.Println(\"test\")"
    ]
  return $ unlines codeLines

-- Extended Type instances
instance Arbitrary TC.Type where
  arbitrary = sized $ \n -> if n <= 0 then
    pure TC.UnknownType
  else oneof
    [ TC.TypeName <$> genGoTypeName
    , TC.TypeFunction <$> listOf (resize (n-1) arbitrary) <*> resize (n-1) arbitrary
    , TC.TypeRecord <$> listOf ((,) <$> genGoVarName <*> resize (n-1) arbitrary)
    , TC.TypeUnion <$> listOf (resize (n-1) arbitrary)
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

instance Arbitrary DepT.TypeDef where
  arbitrary = DepT.TypeDefDecl <$> listOf genIdentifier <*> listOf arbitrary

instance Arbitrary DepT.TypeEnv where
  arbitrary = DepT.TypeEnv <$> arbitrary <*> arbitrary

-- Extended Symbol instances

instance Arbitrary SymbolInfo where
  arbitrary = SymbolInfo
    <$> arbitrary
    <*> frequency [(2, pure Nothing), (3, Just <$> (Dep.TVCon <$> genGoTypeName))]
    <*> frequency [(2, pure Nothing), (3, Just <$> arbitrary)]
    <*> choose (1, 100)
    <*> frequency [(3, pure False), (2, pure True)]
    <*> frequency [(3, pure False), (2, pure True)]
    <*> listOf (Dep.SizeGT <$> (T.pack <$> genIdentifier) <*> choose (0, 100))

-- Arbitrary instances for Ownership module
instance Arbitrary OwnershipType where
  arbitrary = oneof
    [ Owned <$> genIdentifier
    , Borrowed <$> genIdentifier
    , MutBorrowed <$> genIdentifier
    ]

-- Arbitrary instances for Compiler.Errors.Core
instance Arbitrary ErrorSeverity where
  arbitrary = elements [Core.Error, Core.Warning, Core.Info]

instance Arbitrary Core.ErrorCategory where
  arbitrary = elements [Core.TypeChecking, Core.Ownership, Core.Parsing, Core.Semantic, Core.Runtime, Core.Constraint, Core.Inference, Core.Integration, Core.Unknown]

instance Arbitrary Core.ErrorRecovery where
  arbitrary = elements [Core.fatalRecovery, Core.errorRecovery, Core.warningRecovery, Core.infoRecovery]

instance Arbitrary Core.ErrorLocation where
  arbitrary = do
    filePath <- arbitrary
    line <- arbitrary
    column <- arbitrary
    endLine <- arbitrary
    endColumn <- arbitrary
    return $ Core.ErrorLocation filePath line column endLine endColumn

-- Arbitrary instances for DependentTypeError
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

-- Generate base type errors with realistic structure
genTypeError :: Gen Core.TypeError
genTypeError = do
  errId <- genIdentifier
  msg <- genNonEmptyString
  severity <- arbitrary
  category <- arbitrary
  loc <- arbitrary
  recovery <- arbitrary
  let baseError = Core.TypeError errId severity category (T.pack msg) loc Core.emptyContext recovery [] [] [] Nothing
  return $ baseError

-- Extended Error instances with more realistic data
instance Arbitrary OwnershipError where
  arbitrary = oneof
    [ UseAfterMove <$> genGoVarName
    , DoubleMove <$> genGoVarName <*> genGoVarName
    , BorrowWhileMoved <$> genGoVarName
    , MutBorrowWhileBorrowed <$> genGoVarName
    , BorrowWhileMutBorrowed <$> genGoVarName
    , MultipleMutBorrows <$> genGoVarName
    , UseWhileMutBorrowed <$> genGoVarName
    , OutOfScope <$> genGoVarName
    , BorrowError <$> genUniqueIdentifier
    , ParseError <$> genUniqueIdentifier
    , CrossFunctionMove <$> genGoVarName <*> genGoVarName
    , ParameterMoveMismatch <$> genGoVarName
    , ControlFlowError <$> genGoVarName
    , PathSensitiveError <$> genGoVarName
    , LoopOwnershipError <$> genGoVarName
    ]

-- Extended Dependencies instances
instance Arbitrary Dep.TypeVar where
  arbitrary = sized $ \n -> if n <= 0 then
    Dep.TVCon <$> genGoTypeName
  else oneof
    [ Dep.TVCon <$> genGoTypeName
    , Dep.TVVar <$> genUniqueIdentifier
    , Dep.TVApp <$> genUniqueIdentifier <*> listOf (resize (n-1) arbitrary)
    , Dep.TVFun <$> listOf (resize (n-1) arbitrary) <*> resize (n-1) arbitrary
    , Dep.TVTuple <$> listOf (resize (n-1) arbitrary)
    ]

instance Arbitrary Dep.TypeExpr where
  arbitrary = sized $ \n -> if n <= 0 then
    Dep.SimpleT <$> (T.pack <$> genGoTypeName)
  else oneof
    [ Dep.SimpleT <$> (T.pack <$> genGoTypeName)
    , Dep.GenericT <$> (T.pack <$> genGoTypeName) <*> listOf (resize (n-1) arbitrary)
    , Dep.FuncT <$> listOf ((,) <$> (T.pack <$> genIdentifier) <*> resize (n-1) arbitrary) <*> resize (n-1) arbitrary
    , Dep.RefineT <$> resize (n-1) arbitrary <*> listOf (resize (n-1) arbitrary)
    ]

instance Arbitrary Dep.TypeConstraint where
  arbitrary = oneof
    [ Dep.Equal <$> arbitrary <*> arbitrary
    , Dep.Subtype <$> arbitrary <*> arbitrary
    , Dep.Predicate <$> genIdentifier <*> listOf arbitrary
    , Dep.TypeSizeGE <$> arbitrary <*> choose (0, 100)
    , Dep.TypeSizeGT <$> arbitrary <*> choose (0, 100)
    , Dep.TypeRange <$> arbitrary <*> choose (0, 100) <*> choose (0, 100)
    ]

instance Arbitrary Dep.Constraint where
  arbitrary = oneof
    [ Dep.SizeGT <$> (T.pack <$> genIdentifier) <*> choose (0, 100)
    , Dep.SizeGE <$> (T.pack <$> genIdentifier) <*> choose (0, 100)
    , Dep.RangeC <$> (T.pack <$> genIdentifier) <*> choose (0, 100) <*> choose (0, 100)
    , Dep.PredC <$> (T.pack <$> genIdentifier) <*> listOf arbitrary
    ]

instance Arbitrary TC.TypeConstraint where
  arbitrary = oneof
    [ TC.Equal <$> arbitrary <*> arbitrary
    , TC.Subtype <$> arbitrary <*> arbitrary
    , TC.Predicate <$> genIdentifier <*> listOf arbitrary
    , TC.TypeSizeGE <$> arbitrary <*> choose (0, 100)
    , TC.TypeSizeGT <$> arbitrary <*> choose (0, 100)
    , TC.TypeRange <$> arbitrary <*> choose (0, 100) <*> choose (0, 100)
    ]

-- Extended Analyzer instances
instance Arbitrary Own.OwnershipAnalyzer where
  arbitrary = pure $ Own.OwnershipAnalyzer ()

instance Arbitrary DepT.DependentTypeChecker where
  arbitrary = DepT.DependentTypeChecker
    <$> arbitrary
    <*> listOf arbitrary

instance Arbitrary DepAST.DependencyNode where
  arbitrary = DepAST.DependencyNode
    <$> genUniqueIdentifier
    <*> listOf genUniqueIdentifier

instance Arbitrary DepAST.DependencyGraph where
  arbitrary = DepAST.DependencyGraph
    <$> arbitrary

instance Arbitrary AnalysisResult where
  arbitrary = AnalysisResult
    <$> listOf ((,) <$> arbitrary <*> arbitrary)
    <*> listOf ((,) <$> arbitrary <*> arbitrary)
    <*> listOf (Core.OwnershipErrorCombined <$> arbitrary <*> arbitrary)
    <*> listOf genUniqueIdentifier
    <*> listOf genUniqueIdentifier
    <*> pure mempty

instance Arbitrary CombinedError where
  arbitrary = oneof
    [ Core.OwnershipErrorCombined <$> arbitrary <*> arbitrary
    , Core.DependentTypeErrorCombined <$> arbitrary <*> arbitrary
    , Core.IntegrationError <$> genIdentifier <*> arbitrary
    , Core.CrossAnalyzerError <$> genIdentifier <*> arbitrary <*> listOf arbitrary
    ]

instance Arbitrary CompilerError where
  arbitrary = do
    errorId <- genIdentifier
    severity <- arbitrary
    category <- arbitrary
    message <- T.pack <$> genNonEmptyString
    location <- arbitrary
    recovery <- arbitrary
    suggestions <- listOf (T.pack <$> genNonEmptyString)
    timestamp <- pure Nothing
    
    let typeError = Core.TypeError
          { Core.errorId = errorId
          , Core.severity = severity
          , Core.category = category
          , Core.message = message
          , Core.location = location
          , Core.context = Core.emptyContext
          , Core.recovery = recovery
          , Core.suggestions = suggestions
          , Core.relatedErrors = []
          , Core.errorChain = []
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

-- Property test helpers for extended testing
class WellFormedExtended a where
  isWellFormedExtended :: a -> Bool

instance WellFormedExtended ImportDecl where
  isWellFormedExtended (ImportDecl _ path) = 
    not (null path) && 
    all (not . null) (words path) &&
    not (isPrefixOf "." path) -- No relative imports

instance WellFormedExtended GoModule where
  isWellFormedExtended (GoModule _ _pkg imports decls) = 
    all isWellFormedExtended imports && 
    length (nub imports) == length imports && -- No duplicate imports
    all isWellFormedExtended decls

instance WellFormedExtended GoDecl where
  isWellFormedExtended _ = True -- Simplified

instance WellFormedExtended TC.Type where
  isWellFormedExtended (TC.TypeName name) = not (null name)
  isWellFormedExtended (TC.TypeFunction params ret) = 
    all isWellFormedExtended params && isWellFormedExtended ret
  isWellFormedExtended (TC.TypeRecord fields) = 
    all (\(name, t) -> not (null name) && isWellFormedExtended t) fields
  isWellFormedExtended (TC.TypeUnion types) = 
    not (null types) && all isWellFormedExtended types
  isWellFormedExtended TC.UnknownType = True

-- Generators for well-formed extended values
genWellFormedExtendedGoModule :: Gen GoModule
genWellFormedExtendedGoModule = do
  imports <- listOf1 genWellFormedExtendedImportDecl
  let uniqueImports = nub imports -- Remove duplicates
  GoModule
    <$> listOf genUniqueIdentifier
    <*> frequency [(1, pure Nothing), (3, Just <$> (PackageDecl <$> genGoPackageName))]
    <*> pure uniqueImports
    <*> listOf arbitrary

genWellFormedExtendedImportDecl :: Gen ImportDecl
genWellFormedExtendedImportDecl = ImportDecl 
  <$> frequency [(2, pure Nothing), (3, Just <$> genGoVarName)] 
  <*> genGoImportPath

genWellFormedExtendedType :: Gen TC.Type
genWellFormedExtendedType = sized $ \n -> if n <= 0 then
  TC.TypeName <$> genGoTypeName
else oneof
  [ TC.TypeName <$> genGoTypeName
  , TC.TypeFunction <$> listOf (resize (n-1) genWellFormedExtendedType) <*> resize (n-1) genWellFormedExtendedType
  , TC.TypeRecord <$> listOf ((,) <$> genGoVarName <*> resize (n-1) genWellFormedExtendedType)
  , TC.TypeUnion <$> listOf1 (resize (n-1) genWellFormedExtendedType)
  ]

-- Additional utility generators
genNonEmptyList :: Gen a -> Gen [a]
genNonEmptyList gen = sized $ \n -> do
  len <- choose (1, max 1 n)
  vectorOf len gen

genSortedIntList :: Gen [Int]
genSortedIntList = do
  n <- choose (0, 20)
  nums <- vectorOf n (choose (0, 100))
  return $ sort nums

genUniqueStringList :: Gen [String]
genUniqueStringList = do
  n <- choose (0, 10)
  ids <- vectorOf n genUniqueIdentifier
  return $ nub ids

genValidGoCode :: Gen String
genValidGoCode = do
  pkg <- genGoPackageName
  imports <- listOf genGoImportPath
  funcs <- listOf genGoFunctionDecl
  vars <- listOf genGoVarDecl
  types <- listOf genGoTypeDecl
  
  let importLines = map (\path -> "import \"" ++ path ++ "\"") imports
      code = unlines $ ["package " ++ pkg] ++ importLines ++ funcs ++ vars ++ types ++ ["}"]
  
  return code

-- Property test utilities
prop_roundtrip_consistency :: (Eq a, Show a) => (a -> String) -> (String -> Maybe a) -> a -> Property
prop_roundtrip_consistency toString fromString x = 
  case fromString (toString x) of
    Nothing -> property False
    Just x' -> x === x'

prop_idempotent :: (Eq a, Show a) => (a -> a) -> a -> Property
prop_idempotent f x = f (f x) === f x

prop_associative :: (Eq a, Show a) => (a -> a -> a) -> a -> a -> a -> Property
prop_associative f x y z = f (f x y) z === f x (f y z)

prop_commutative :: (Eq a, Show a) => (a -> a -> a) -> a -> a -> Property
prop_commutative f x y = f x y === f y x
