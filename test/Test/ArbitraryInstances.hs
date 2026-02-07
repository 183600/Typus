{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ArbitraryInstances where

import Test.Tasty.QuickCheck (Arbitrary(..), oneof, elements, choose, listOf, resize)
import qualified Data.Text as T

import Compiler (CompilerError(..), CompilationPhase(..))
import qualified Compiler.Errors.Core as Core (errorWithCategory, ErrorCategory(..), ErrorLocation(..), message, TypeError(..), ErrorSeverity(..), ErrorContext(..), ErrorRecovery(..))
import qualified Compiler.TypeChecker as TC (TypeCheckDiagnostic(..), TypeError(..))
import Compiler.Errors.Types ()
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
    let defaultLoc = Core.ErrorLocation Nothing 0 0 Nothing Nothing
    let typeError = Core.errorWithCategory "TEST001" Core.Parsing (T.pack "Test error") defaultLoc
    return $ CompilerError typeError Nothing [] phase

instance Arbitrary Core.TypeError where
  arbitrary = do
    errId <- choose (1000, 9999 :: Int) >>= \n -> return ("E" ++ show n)
    errSeverity <- arbitrary
    errCategory <- arbitrary
    errMsg <- arbitrary
    errLocation <- arbitrary
    errContext <- arbitrary
    errRecovery <- arbitrary
    -- Memory optimization: limit list sizes to prevent excessive memory usage
    errSuggestions <- resize 3 $ listOf arbitrary
    errRelatedErrors <- resize 2 $ listOf arbitrary
    errErrorChain <- resize 2 $ listOf arbitrary
    errTimestamp <- arbitrary
    return $ Core.TypeError
      { Core.errorId = errId
      , Core.severity = errSeverity
      , Core.category = errCategory
      , Core.message = errMsg
      , Core.location = errLocation
      , Core.context = errContext
      , Core.recovery = errRecovery
      , Core.suggestions = errSuggestions
      , Core.relatedErrors = errRelatedErrors
      , Core.errorChain = errErrorChain
      , Core.timestamp = errTimestamp
      }

instance Arbitrary TC.TypeError where
  arbitrary = do
    ctx <- arbitrary
    msg <- elements ["Type mismatch", "Undefined variable", "Invalid operation"]
    return $ TC.TypeError
      { TC.teContext = ctx
      , TC.teMessage = msg
      }

instance Arbitrary TC.TypeCheckDiagnostic where
  arbitrary = do
    hasErrs <- arbitrary
    ctx <- if hasErrs then return (Just "context") else return Nothing
    detail <- arbitrary
    return $ TC.TypeCheckDiagnostic ctx detail

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Core.ErrorSeverity where
  arbitrary = elements [Core.Fatal, Core.Error, Core.Warning, Core.Info]

instance Arbitrary Core.ErrorCategory where
  arbitrary = elements [Core.Parsing, Core.TypeChecking, Core.Semantic, Core.Runtime]

instance Arbitrary Core.ErrorLocation where
  arbitrary = do
    filePath' <- arbitrary
    line' <- choose (1, 100)
    column' <- choose (1, 100)
    endLine' <- arbitrary
    endColumn' <- arbitrary
    return $ Core.ErrorLocation filePath' line' column' endLine' endColumn'

instance Arbitrary Core.ErrorContext where
  arbitrary = do
    ctxCode <- arbitrary
    ctxFunction <- arbitrary
    ctxVariable <- arbitrary
    ctxType <- arbitrary
    -- Memory optimization: limit additional context to prevent excessive memory usage
    ctxAdditional <- resize 3 $ listOf arbitrary
    return $ Core.ErrorContext ctxCode ctxFunction ctxVariable ctxType ctxAdditional

instance Arbitrary Core.ErrorRecovery where
  arbitrary = do
    canRec <- arbitrary
    shouldCont <- arbitrary
    recAction <- arbitrary
    recHint <- arbitrary
    recCost <- choose (0, 100)
    recConfidence <- choose (0.0, 1.0)
    return $ Core.ErrorRecovery canRec shouldCont recAction recHint recCost recConfidence

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