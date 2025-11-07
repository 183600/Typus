{-# LANGUAGE OverloadedStrings #-}
module Analyzer.DependentTypeBridge (
    runDependentTypeAnalysis,
    extractTypeDefinitions
) where

import Analyzer.State
import Analyzer.SymbolTable (trim)
import Analyzer.Types
import qualified Dependencies as Dep
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..), parseTypus)
import SourceLocation (Located, locatedValue)
import qualified Compiler.DependentTypeChecker as DepChecker

import Control.Monad.State
import Data.List (isInfixOf, isPrefixOf)
import qualified Data.Map.Strict as Map

runDependentTypeAnalysis :: String -> IntegratedAnalyzer [(ErrorSeverity, Dep.DependentTypeError)]
runDependentTypeAnalysis code =
    case parseTypus code of
        Left err -> do
            let parseError = Dep.ParseError err
            addDependentTypeError Error parseError
            pure [(Error, parseError)]
        Right typusFile -> do
            let dependentContent = DepChecker.extractDependentTypeContent typusFile
            if not (dependentTypesEnabled typusFile) || null (trim dependentContent)
                then pure []
                else do
                    let typeDefinitions = extractTypeDefinitions dependentContent
                        tc = Dep.newDependentTypeCheckerWithTypes typeDefinitions
                    modify $ \s -> s { dependentTypeChecker = tc }
                    _ <- gets dependentTypeChecker
                    let typeErrors = Dep.analyzeDependentTypes dependentContent
                        filteredErrors = filterKnownTypeErrors typeErrors
                    updateSymbolTableWithTypes filteredErrors
                    symbols <- gets symbolTable
                    let significantErrors = filterSignificantTypeErrors filteredErrors symbols
                        labeledErrors = map (\err -> (Error, err)) significantErrors
                    mapM_ (uncurry addDependentTypeError) labeledErrors
                    pure labeledErrors

updateSymbolTableWithTypes :: [Dep.DependentTypeError] -> IntegratedAnalyzer ()
updateSymbolTableWithTypes _typeErrors = pure ()

filterSignificantTypeErrors :: [Dep.DependentTypeError] -> Map.Map String SymbolInfo -> [Dep.DependentTypeError]
filterSignificantTypeErrors errors _symbols = filter isSignificant errors
  where
    isSignificant (Dep.DependentTypeMismatch t1 t2) = show t1 /= "" && show t2 /= ""
    isSignificant _ = True

filterKnownTypeErrors :: [Dep.DependentTypeError] -> [Dep.DependentTypeError]
filterKnownTypeErrors = filter isSignificantTypeError
  where
    isSignificantTypeError (Dep.TypeNotFound typeName) = typeName `notElem` knownTypes
    isSignificantTypeError _ = True

    knownTypes :: [String]
    knownTypes =
        [ "int", "string", "bool", "float64", "byte", "rune"
        , "error", "interface{}", "[]int", "[]T", "[]string"
        , "Vector", "NonEmptySlice", "map", "chan", "func"
        ]

dependentTypesEnabled :: TypusFile -> Bool
dependentTypesEnabled typusFile =
    let directives = tfDirectives typusFile
        blocks = tfBlocks typusFile
    in directiveEnabled (fdDependentTypes directives)
        || any (directiveEnabled . bdDependentTypes . cbDirectives) blocks

directiveEnabled :: Maybe (Located Bool) -> Bool
directiveEnabled = maybe False locatedValue

extractTypeDefinitions :: String -> [(String, [String], [Dep.TypeConstraint])]
extractTypeDefinitions code =
    let linesOfCode = lines code
        typeLines = filter isTypeDefinitionLine linesOfCode
    in map parseTypeDefinitionLine typeLines
  where
    isTypeDefinitionLine line =
        let trimmed = trim line
        in "type " `isPrefixOf` trimmed && not ("//" `isPrefixOf` trimmed)

    parseTypeDefinitionLine line =
        let withoutType = drop 5 (trim line)
            (typeName, rest) = break (`elem` [' ', '<']) withoutType
        in if null typeName
               then ("", [], [])
               else
                   if "<" `isInfixOf` rest
                       then parseGenericTypeDefinition typeName rest
                       else parseSimpleTypeDefinition typeName rest

    parseGenericTypeDefinition typeName rest =
        let paramsPart = takeWhile (/= '>') (drop 1 rest)
            params = map trim (splitByComma paramsPart)
            afterParams = dropWhile (/= '>') rest
            cs = parseWhereConstraints afterParams
        in (typeName, params, cs)

    parseSimpleTypeDefinition typeName rest =
        (typeName, [], parseWhereConstraints rest)

    parseWhereConstraints rest =
        let trimmed' = trim rest
        in if "where " `isPrefixOf` trimmed'
               then parseSimpleConstraints (drop 6 trimmed')
               else []

    parseSimpleConstraints constraintStr =
        let constraints' = splitByChar '&' constraintStr
        in map parseSingleConstraint constraints'

    parseSingleConstraint constraint =
        let trimmed' = trim constraint
            wordsInConstraint = words trimmed'
        in case wordsInConstraint of
            [var, ">", value] ->
                case reads value of
                    [(num, "")] -> Dep.TypeSizeGT (Dep.TVVar var) (num + 1)
                    _ -> Dep.Predicate trimmed' [Dep.TVVar var]
            [var, ">=", value] ->
                case reads value of
                    [(num, "")] -> Dep.TypeSizeGE (Dep.TVVar var) num
                    _ -> Dep.Predicate trimmed' [Dep.TVVar var]
            ["len", var, ">", value] ->
                case reads value of
                    [(num, "")] -> Dep.TypeSizeGT (Dep.TVVar var) (num + 1)
                    _ -> Dep.Predicate trimmed' [Dep.TVVar var]
            ["nonempty", var] -> Dep.TypeSizeGT (Dep.TVVar var) 0
            _ -> Dep.Predicate trimmed' []

    splitByComma s = case break (== ',') s of
        (a, []) -> [a]
        (a, _ : b) -> a : splitByComma b

splitByChar :: Char -> String -> [String]
splitByChar delimiter s = case break (== delimiter) s of
    (a, []) -> [a]
    (a, _ : b) -> a : splitByChar delimiter b
