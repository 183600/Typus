module Compiler.DependentTypeChecker (
    checkDependentTypes,
    extractDependentTypeContent
) where

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import DependentTypesParser (DependentTypeError(..), runDependentTypesParser, parserErrors)
import Compiler.Error
import SourceLocation (SourceLocation, Located(..), locatedValue, sourceLocation)

import Data.List (intercalate)
import Data.Maybe (catMaybes)

directiveEnabled :: Maybe (Located Bool) -> Bool
directiveEnabled = maybe False locatedValue

checkDependentTypes :: TypusFile -> Either CompilationError ()
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
                       Left err -> Left $ mkCompilationError DependentTypeErrorKind ("Dependent type parsing error: " ++ err) []
                       Right (_, parser) ->
                           let errors = parserErrors parser
                           in if null errors
                                  then Right ()
                                  else
                                      let (msg, locs) = formatDependentTypeErrors errors
                                      in Left $ mkCompilationError DependentTypeErrorKind ("Dependent type errors: " ++ msg) locs
           else Right ()

extractDependentTypeContent :: TypusFile -> String
extractDependentTypeContent typusFile =
    let dependentBlocks = filter (directiveEnabled . bdDependentTypes . cbDirectives) (tfBlocks typusFile)
    in concatMap cbContent dependentBlocks

formatDependentTypeErrors :: [DependentTypeError] -> (String, [SourceLocation])
formatDependentTypeErrors errs =
    let formatted = map format errs
        message = intercalate "; " (map fst formatted)
        locations = catMaybes (map snd formatted)
    in (message, locations)
  where
    format (SyntaxError msg line snippet) =
        let base = "Syntax error at line " ++ show line ++ ": " ++ msg ++ if null snippet then "" else " (" ++ snippet ++ ")"
            loc = if line > 0 then Just (sourceLocation Nothing (Just line) Nothing) else Nothing
        in (base, loc)
    format (InvalidTypeSyntax msg) = ("Invalid type syntax: " ++ msg, Nothing)
    format (MissingConstraint msg) = ("Missing constraint: " ++ msg, Nothing)
    format (InvalidParameter msg) = ("Invalid parameter: " ++ msg, Nothing)
    format (ConstraintParseError msg) = ("Constraint parse error: " ++ msg, Nothing)
    format (TypeVariableError msg) = ("Type variable error: " ++ msg, Nothing)
