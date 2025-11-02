module Compiler
  ( compile
  , CompilationError(..)
  , ErrorKind(..)
  , SourceLocation(..)
  , renderCompilationError
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

import Parser (TypusFile(..))
import Compiler.GoAst (renderGoModule)
import qualified Compiler.IR as IR
import Compiler.Error
import Compiler.DependentTypeChecker (checkDependentTypes)
import Compiler.OwnershipChecker (checkOwnership)
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

compile :: TypusFile -> Either CompilationError String
compile typusFile = do
  sourceIR <- ensureSourceIR typusFile
  let parsedFile = IR.sourceTypusFile sourceIR
  checkDependentTypes parsedFile
  ensureNoTypeErrors parsedFile
  checkOwnership parsedFile
  semanticIR <- IR.buildSemanticIR sourceIR
  let goArtifact = IR.emitGo semanticIR
  pure (IR.goSource goArtifact)
  where
    ensureNoTypeErrors file =
      if hasTypeErrors file
        then Left $ mkCompilationError TypeErrorKind "Type errors detected" []
        else Right ()

ensureSourceIR :: TypusFile -> Either CompilationError IR.SourceIR
ensureSourceIR typusFile =
  if hasMalformedSyntax typusFile
    then Left $ mkCompilationError SyntaxErrorKind "Malformed syntax detected" []
    else Right (IR.buildSourceIR typusFile)

generateGoCode :: TypusFile -> String
generateGoCode typusFile =
  case IR.moduleFromTypus typusFile of
    Left _         -> IR.rawSourceFromTypus typusFile
    Right goModule -> renderGoModule goModule
