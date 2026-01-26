{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ArbitraryInstances where

import Test.Tasty.QuickCheck (Arbitrary(..), oneof, elements, choose, listOf)
import qualified Data.Text as T

import Compiler (CompilerError(..), CompilationPhase(..))
import Compiler.Errors.Core (errorWithCategory, ErrorCategory(..), ErrorLocation(..), message, TypeError(..))
import Compiler.TypeChecker (TypeCheckDiagnostic(..), TypeError(..))
import Compiler.Errors.Types (ErrorSeverity(..), ErrorContext(..), ErrorRecovery(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Parser (TypusFile(..), CodeBlock(..), BlockDirectives(..), FileDirectives(..))
import qualified SyntaxValidator as SyntaxValidator

-- ============================================================================
-- Arbitrary Instances for Compiler Types
-- ============================================================================

instance Arbitrary CompilationPhase where
  arbitrary = oneof 
    [ return ParsingPhase
    , return TypeCheckingPhase
    , return CodeGenerationPhase
    ]

instance Arbitrary CompilerError where
  arbitrary = do
    phase <- arbitrary
    -- Generate a minimal CompilerError for testing
    let defaultLoc = ErrorLocation Nothing 0 0 Nothing Nothing
    let typeError = errorWithCategory "TEST001" Parsing (T.pack "Test error") defaultLoc
    return $ CompilerError typeError Nothing [] phase

instance Arbitrary Compiler.Errors.Core.TypeError where
  arbitrary = do
    errId <- choose (1000, 9999 :: Int) >>= \n -> return ("E" ++ show n)
    errSeverity <- arbitrary
    errCategory <- arbitrary
    errMsg <- arbitrary
    errLocation <- arbitrary
    errContext <- arbitrary
    errRecovery <- arbitrary
    errSuggestions <- listOf arbitrary
    errRelatedErrors <- listOf arbitrary
    errErrorChain <- listOf arbitrary
    errTimestamp <- arbitrary
    return $ Compiler.Errors.Core.TypeError
      { Compiler.Errors.Core.errorId = errId
      , Compiler.Errors.Core.severity = errSeverity
      , Compiler.Errors.Core.category = errCategory
      , Compiler.Errors.Core.message = errMsg
      , Compiler.Errors.Core.location = errLocation
      , Compiler.Errors.Core.context = errContext
      , Compiler.Errors.Core.recovery = errRecovery
      , Compiler.Errors.Core.suggestions = errSuggestions
      , Compiler.Errors.Core.relatedErrors = errRelatedErrors
      , Compiler.Errors.Core.errorChain = errErrorChain
      , Compiler.Errors.Core.timestamp = errTimestamp
      }

instance Arbitrary Compiler.TypeChecker.TypeError where
  arbitrary = do
    ctx <- arbitrary
    msg <- elements ["Type mismatch", "Undefined variable", "Invalid operation"]
    return $ Compiler.TypeChecker.TypeError
      { teContext = ctx
      , teMessage = msg
      }

instance Arbitrary TypeCheckDiagnostic where
  arbitrary = do
    hasErrs <- arbitrary
    ctx <- if hasErrs then return (Just "context") else return Nothing
    detail <- arbitrary
    return $ TypeCheckDiagnostic ctx detail

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary ErrorSeverity where
  arbitrary = elements [Fatal, Error, Warning, Info]

instance Arbitrary ErrorCategory where
  arbitrary = elements [Parsing, TypeChecking, Semantic, Runtime]

instance Arbitrary ErrorLocation where
  arbitrary = do
    filePath' <- arbitrary
    line' <- choose (1, 100)
    column' <- choose (1, 100)
    endLine' <- arbitrary
    endColumn' <- arbitrary
    return $ ErrorLocation filePath' line' column' endLine' endColumn'

instance Arbitrary ErrorContext where
  arbitrary = do
    ctxCode <- arbitrary
    ctxFunction <- arbitrary
    ctxVariable <- arbitrary
    ctxType <- arbitrary
    ctxAdditional <- listOf arbitrary
    return $ ErrorContext ctxCode ctxFunction ctxVariable ctxType ctxAdditional

instance Arbitrary ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    recAction <- arbitrary
    recHint <- arbitrary
    recCost <- choose (0, 100)
    recConfidence <- choose (0.0, 1.0)
    return $ ErrorRecovery canRec shouldCont recAction recHint recCost recConfidence

-- Additional Arbitrary instances
instance Arbitrary Parser.FileDirectives where
  arbitrary = do
    ownership <- arbitrary
    dependentTypes <- arbitrary
    constraints <- arbitrary
    return $ Parser.FileDirectives ownership dependentTypes constraints

instance Arbitrary Parser.CodeBlock where
  arbitrary = do
    directives' <- arbitrary
    content' <- arbitrary
    span' <- arbitrary
    return $ Parser.CodeBlock directives' content' span'

instance Arbitrary Parser.BlockDirectives where
  arbitrary = do
    own <- arbitrary
    depTypes <- arbitrary
    constr <- arbitrary
    return $ Parser.BlockDirectives own depTypes constr

instance Arbitrary Parser.TypusFile where
  arbitrary = do
    directives' <- arbitrary
    buildTags' <- arbitrary
    blocks' <- arbitrary
    syntaxErrors' <- arbitrary
    return $ Parser.TypusFile directives' buildTags' blocks' syntaxErrors'

instance Arbitrary a => Arbitrary (SourceLocation.Located a) where
  arbitrary = do
    val <- arbitrary
    pos' <- arbitrary
    span' <- arbitrary
    return $ SourceLocation.Located val pos' span'

instance Arbitrary SyntaxValidator.ErrorType where
  arbitrary = elements 
    [ SyntaxValidator.MissingBrace
    , SyntaxValidator.MissingParenthesis
    , SyntaxValidator.MissingBracket
    , SyntaxValidator.UnclosedString
    , SyntaxValidator.UnclosedComment
    , SyntaxValidator.InvalidIdentifier
    , SyntaxValidator.InvalidTypeDeclaration
    , SyntaxValidator.InvalidFunctionDeclaration
    , SyntaxValidator.InvalidImport
    , SyntaxValidator.InvalidStatement
    , SyntaxValidator.UnterminatedBlock
    , SyntaxValidator.InvalidOperator
    , SyntaxValidator.MissingSemicolon
    , SyntaxValidator.UnexpectedToken
    , SyntaxValidator.MissingPackageDeclaration
    , SyntaxValidator.DuplicateDeclaration
    , SyntaxValidator.InvalidBlockStructure
    , SyntaxValidator.UndeclaredVariable
    , SyntaxValidator.SyntaxWarning
    ]

instance Arbitrary SyntaxValidator.SyntaxError where
  arbitrary = do
    errType <- arbitrary
    errMsg <- arbitrary
    lineNum <- arbitrary
    colNum <- arbitrary
    lineCont <- arbitrary
    return $ SyntaxValidator.SyntaxError errType errMsg lineNum colNum lineCont

-- Helper function to generate SourceSpan
genSourceSpan :: SourceLocation.SourceSpan
genSourceSpan = SourceLocation.SourceSpan (SourceLocation.SourcePos 1 1 0) (SourceLocation.SourcePos 1 1 0)