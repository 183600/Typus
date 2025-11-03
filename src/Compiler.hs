module Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , renderCompilationError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , hasTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  , checkDependentTypes
  , checkOwnership
  , generateGoCode
  ) where

import qualified Data.Text as T

import Parser (TypusFile(..))
import Compiler.GoAst (renderGoModule)
import qualified Compiler.IR as IR
import Compiler.DependentTypeChecker (checkDependentTypes)
import Compiler.OwnershipChecker (checkOwnership, checkOwnershipWithValueInfo)
import Compiler.TypeChecker
  ( Type(..)
  , TypeEnv(..)
  , hasTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  )
import EnhancedErrorHandler
  ( CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  )
import ErrorHandler (ErrorCategory(..), ErrorSeverity(..))
import Compiler.EnhancedErrors (mkCompilerError, defaultSpan)

-- | Compile a parsed Typus file into Go code while producing enhanced diagnostics.
compile :: TypusFile -> CompilerResult String
compile typusFile = do
  sourceIR <- ensureSourceIR typusFile
  semanticIR <- IR.buildSemanticIR sourceIR
  let parsedFile = IR.sourceTypusFile sourceIR
  checkDependentTypes parsedFile
  ensureNoTypeErrors parsedFile
  checkOwnershipWithValueInfo parsedFile (IR.semanticValueInfo semanticIR)
  let goArtifact = IR.emitGo semanticIR
  pure (IR.goSource goArtifact)
  where
    ensureNoTypeErrors file =
      if hasTypeErrors file
        then Left [typeCheckFailure]
        else Right ()

-- | Convert legacy error lists into a human readable form.
renderCompilationError :: [CompilerError] -> String
renderCompilationError = formatCompilerErrors

-- | Ensure the parsed Typus file is well-formed before entering the semantic pipeline.
ensureSourceIR :: TypusFile -> CompilerResult IR.SourceIR
ensureSourceIR typusFile =
  if hasMalformedSyntax typusFile
    then Left [malformedSyntaxError]
    else Right (IR.buildSourceIR typusFile)

-- | Reuse the Go emission helpers but fall back to the raw Typus content if
-- semantic lowering fails. This keeps the function total for tooling that only
-- needs best-effort output.
generateGoCode :: TypusFile -> String
generateGoCode typusFile =
  case IR.moduleFromTypus typusFile of
    Left _         -> IR.rawSourceFromTypus typusFile
    Right goModule -> renderGoModule goModule

-- ---------------------------------------------------------------------------
-- Enhanced error builders for core compiler stages
-- ---------------------------------------------------------------------------

malformedSyntaxError :: CompilerError
malformedSyntaxError =
  mkCompilerError
    "CP0001"
    (T.pack "Malformed syntax detected in Typus source")
    ParsingPhase
    Parsing
    Error
    (Just defaultSpan)
    Nothing
    (map T.pack
      [ "Verify all braces, brackets, and parentheses are balanced"
      , "Check that directives such as //! ownership are correctly closed"
      ])
    ["Compiler.ensureSourceIR"]
    Nothing

typeCheckFailure :: CompilerError
typeCheckFailure =
  mkCompilerError
    "CP0002"
    (T.pack "Type errors detected during semantic analysis")
    TypeCheckingPhase
    TypeChecking
    Error
    (Just defaultSpan)
    Nothing
    (map T.pack
      [ "Inspect intermediate type-checking diagnostics for precise locations"
      , "Add explicit type annotations to clarify intent"
      ])
    ["Compiler.ensureNoTypeErrors"]
    Nothing
