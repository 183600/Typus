module Compiler.DependentTypeChecker (
    checkDependentTypes,
    extractDependentTypeContent
) where

import qualified Data.Text as T

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import DependentTypesParser (DependentTypeError(..), runDependentTypesParser, parserErrors)
import Compiler.Errors
    ( CompilerError
    , CompilerResult
    , CompilationPhase(..)
    , ErrorCategory(..)
    , ErrorSeverity(..)
    , mkCompilerError
    )
import SourceLocation
    ( Located(..)
    , SourceSpan
    , locatedValue
    , posAt
    , spanFrom
    )

directiveEnabled :: Maybe (Located Bool) -> Bool
directiveEnabled = maybe False locatedValue

checkDependentTypes :: TypusFile -> CompilerResult ()
checkDependentTypes typusFile =
    let directives = tfDirectives typusFile
        blocks = tfBlocks typusFile
        fileEnabled = directiveEnabled (fdDependentTypes directives)
        blockEnabled = any (directiveEnabled . bdDependentTypes . cbDirectives) blocks
        shouldCheck = fileEnabled || blockEnabled
    in if shouldCheck
           then case extractDependentTypeContent typusFile of
               [] -> Right ()
               content ->
                   case runDependentTypesParser content of
                       Left err -> Left [parserFailure err]
                       Right (_, parser) ->
                           let errors = parserErrors parser
                           in if null errors
                                  then Right ()
                                  else Left (map toCompilerError errors)
           else Right ()

extractDependentTypeContent :: TypusFile -> String
extractDependentTypeContent typusFile =
    let directives = tfDirectives typusFile
        fileEnabled = directiveEnabled (fdDependentTypes directives)
        blocks = tfBlocks typusFile
        includeBlock block =
            case bdDependentTypes (cbDirectives block) of
                Nothing -> fileEnabled
                Just locatedFlag -> locatedValue locatedFlag
    in concatMap cbContent (filter includeBlock blocks)

parserFailure :: String -> CompilerError
parserFailure errMsg =
    mkCompilerError
        "DT0000"
        (T.pack ("Failed to parse dependent type block: " ++ errMsg))
        DependentTypeCheckingPhase
        Constraint
        Error
        Nothing
        Nothing
        (map T.pack
            [ "Ensure dependent type blocks follow the documented syntax"
            , "Check for unmatched braces or directives"
            ])
        []
        Nothing

toCompilerError :: DependentTypeError -> CompilerError
toCompilerError err = case err of
    SyntaxError msg line snippet ->
        mkCompilerError
            "DT0001"
            (T.pack ("Syntax error: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            (Just (spanForLine line))
            (nonEmpty snippet)
            (map T.pack
                [ "Review the syntax near the reported line"
                , "Ensure nested constructs are properly closed"
                ])
            []
            Nothing
    InvalidTypeSyntax msg ->
        mkCompilerError
            "DT0002"
            (T.pack ("Invalid type syntax: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Verify type declarations inside dependent type blocks"])
            []
            Nothing
    MissingConstraint msg ->
        mkCompilerError
            "DT0003"
            (T.pack ("Missing constraint: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Add the required constraint to the dependent type declaration"])
            []
            Nothing
    InvalidParameter msg ->
        mkCompilerError
            "DT0004"
            (T.pack ("Invalid parameter: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Check the parameter list for the dependent type"])
            []
            Nothing
    ConstraintParseError msg ->
        mkCompilerError
            "DT0005"
            (T.pack ("Constraint parse error: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Ensure constraints use supported operators and syntax"])
            []
            Nothing
    TypeVariableError msg ->
        mkCompilerError
            "DT0006"
            (T.pack ("Type variable error: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Ensure type variables are declared before use"])
            []
            Nothing

spanForLine :: Int -> SourceSpan
spanForLine lineNumber =
    let safeLine = max 1 lineNumber
    in spanFrom (posAt safeLine 1)

nonEmpty :: String -> Maybe String
nonEmpty s = if null s then Nothing else Just s
