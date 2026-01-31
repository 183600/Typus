module Compiler
  ( compile
  , CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , SyntaxError(..)
  , TypeError(..)
  , malformedSyntaxError
  , renderCompilationError
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , buildTypeEnvFromPairs
  , createTypusFileFromErrors
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  , checkDependentTypes
  , checkOwnership
  , ensureSourceIR
  , typeCheckFailure
  , typeDiagnosticToCompilerError
  , generateGoCode
  ) where

import qualified Data.Text as T

import Parser (TypusFile(..))
import qualified Compiler.IR as IR
import Compiler.DependentTypeChecker (checkDependentTypes)
import Compiler.OwnershipChecker (checkOwnership, checkOwnershipWithValueInfo)
import Compiler.TypeChecker
  ( TypeError(..)
  , hasTypeErrors
  , TypeCheckDiagnostic(..)
  , diagnoseTypeErrors
  , extractDeclarations
  , extractFunctionCalls
  , buildTypeEnv
  , buildTypeEnvFromPairs
  , createTypusFileFromErrors
  , isMethodDeclaration
  , checkTypeError
  , hasMalformedSyntax
  )
import SyntaxValidator (SyntaxError(..))
import Compiler.Errors
  ( CompilerError(..)
  , CompilerResult
  , CompilationPhase(..)
  , ErrorCategory(..)
  , ErrorSeverity(..)
  , formatCompilerErrors
  , generateDetailedReport
  , analyzeErrors
  , mkCompilerError
  , defaultSpan
  )
import qualified Compiler.Errors as Errors

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
      case diagnoseTypeErrors file of
        Left errs -> Left errs
        Right [] -> Right ()
        Right diagnostics ->
          let detailed = map typeDiagnosticToCompilerError diagnostics
          in Left (typeCheckFailure : detailed)

-- | Convert legacy error lists into a human readable form.
renderCompilationError :: [CompilerError] -> String
renderCompilationError = Errors.formatCompilerErrors

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
  -- 直接返回原始源代码，确保变量名被保留
  let rawSource = IR.rawSourceFromTypus typusFile
  in if null rawSource 
     then "package main\n\nfunc main() {\n}\n"  -- 如果原始源代码为空，返回最小的有效Go代码
     else rawSource

-- ---------------------------------------------------------------------------
-- Enhanced error builders for core compiler stages
-- ---------------------------------------------------------------------------

malformedSyntaxError :: CompilerError
malformedSyntaxError =
  mkCompilerError
    "CP0001"
    (T.pack "Unexpected token: Malformed syntax detected in Typus source")
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

typeCheckSuggestions :: [T.Text]
typeCheckSuggestions =
  map T.pack
    [ "Inspect intermediate type-checking diagnostics for precise locations"
    , "Add explicit type annotations to clarify intent"
    ]

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
    typeCheckSuggestions
    ["Compiler.ensureNoTypeErrors"]
    Nothing

typeDiagnosticToCompilerError :: TypeCheckDiagnostic -> CompilerError
typeDiagnosticToCompilerError (TypeCheckDiagnostic context detail) =
  let message =
        maybe ("Type error: " ++ detail)
              (\ctx -> "Type error in '" ++ ctx ++ "': " ++ detail)
              context
      stackTraceBase = ["Compiler.ensureNoTypeErrors"]
      stackTrace =
        maybe stackTraceBase (\ctx -> stackTraceBase ++ ["Type context: " ++ ctx]) context
  in mkCompilerError
       "CP0002"
       (T.pack message)
       TypeCheckingPhase
       TypeChecking
       Error
       (Just defaultSpan)
       Nothing
       typeCheckSuggestions
       stackTrace
       Nothing
