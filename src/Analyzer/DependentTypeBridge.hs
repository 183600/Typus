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
import DependentTypesParser
    ( DependentType(..)
    , TypeConstraint(..)
    , TypeParameter(..)
    , TypeRef(..)
    , runDependentTypesParser
    )

import Control.Monad.State
import Data.Char (isAlpha, isUpper)
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
    case runDependentTypesParser code of
        Left _ -> []
        Right (definitions, _) -> mapMaybe toTypeDefinition definitions
  where
    toTypeDefinition :: DependentType -> Maybe (String, [String], [Dep.TypeConstraint])
    toTypeDefinition (TypeDecl name params _ declConstraints) =
        let paramNames = map paramName params
            scope = Set.fromList paramNames
            parameterConstraints = concatMap (collectParameterConstraints scope) params
            declarationConstraints = concatMap (convertConstraint scope) declConstraints
        in Just (name, paramNames, parameterConstraints <> declarationConstraints)
    toTypeDefinition _ = Nothing

    collectParameterConstraints scope TypeParameter{ paramName = pname, paramType = pType, paramConstraints = pcs } =
        maybeToList (paramTypeConstraint scope pname pType)
            <> concatMap (convertConstraint scope) pcs

    paramTypeConstraint scope pname pType
        | pType == defaultParamType = Nothing
        | otherwise = Just (Dep.Subtype (Dep.TVVar pname) (convertTypeRef scope pType))

    defaultParamType = TypeRef "int" []

    convertConstraint scope constraint =
        case constraint of
            EqualityConstraint lhs rhs ->
                [Dep.Equal (Dep.TVVar lhs) (valueToTypeVar scope rhs)]
            RangeConstraint name low high ->
                [Dep.TypeRange (Dep.TVVar name) low high]
            SizeConstraint name threshold ->
                [Dep.TypeSizeGE (Dep.TVVar name) threshold]
            NonEmptyConstraint name ->
                [Dep.TypeSizeGT (Dep.TVVar name) 0]
            PredicateConstraint name args ->
                [Dep.Predicate name (map (valueToTypeVar scope) args)]
            TypeClassConstraint name typeRef ->
                [Dep.Subtype (Dep.TVVar name) (convertTypeRef scope typeRef)]
            CustomConstraint raw _ ->
                [Dep.Predicate raw []]

    valueToTypeVar scope text =
        case reads text :: [(Integer, String)] of
            [(n, "")] -> Dep.TVCon (show n)
            _
                | Set.member text scope -> Dep.TVVar text
                | isBuiltinTypeName text -> Dep.TVCon text
                | startsWithUpper text -> Dep.TVCon text
                | otherwise -> Dep.TVVar text

    convertTypeRef scope (TypeRef name args)
        | null args =
            if Set.member name scope
                then Dep.TVVar name
                else Dep.TVCon name
        | otherwise =
            Dep.TVApp name (map (convertTypeRef scope) args)

    startsWithUpper name =
        case name of
            c:rest -> isUpper c && any isAlpha rest
            _ -> False

    isBuiltinTypeName name = name `elem` builtinTypeNames

    builtinTypeNames :: [String]
    builtinTypeNames =
        [ "int", "string", "bool", "float64", "byte", "rune"
        , "error", "interface{}", "map", "chan", "func"
        ]

