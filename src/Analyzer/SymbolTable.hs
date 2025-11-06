{-# LANGUAGE RecordWildCards #-}

module Analyzer.SymbolTable (
    collectSymbolsAndTypes,
    collectSymbolsFromAST,
    trim,
    isReservedName,
    extractTypeEnvironment
) where

import Analyzer.Types
import qualified Dependencies as Dep
import qualified Ownership as Own
import Compiler.GoAst (GoModule(..), GoDecl(..), FuncDecl(..), VarDecl(..), ConstDecl(..), TypeDecl(..), parseGoModule)

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Char (isSpace, isAlphaNum, isDigit)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)

type SymbolTable = Map.Map String SymbolInfo
type SymbolCollector a = IntegratedAnalyzer a

collectSymbolsAndTypes :: String -> IntegratedAnalyzer SymbolTable
collectSymbolsAndTypes code = do
    case parseGoModule (lines code) of
        Right goModule -> collectSymbolsFromAST goModule
        Left _ -> do
            let linesOfCode = lines code
            symbols <- mapM processLineForSymbols (zip [1 ..] linesOfCode)
            let combinedSymbols = foldr Map.union Map.empty symbols
            validateSymbolTable combinedSymbols

collectSymbolsFromAST :: GoModule -> IntegratedAnalyzer SymbolTable
collectSymbolsFromAST GoModule{..} = do
    symbols <- mapM processDecl (zip [1..] gmDecls)
    let combinedSymbols = foldr Map.union Map.empty symbols
    validateSymbolTable combinedSymbols
  where
    processDecl (lineNum, GoFunc (FuncDecl ls)) = case ls of
        [] -> pure Map.empty
        (h:_) -> processFunctionDeclaration lineNum h
    processDecl (lineNum, GoVar (VarDecl ls _)) = do
        varSymbols <- mapM (uncurry processVariableDeclaration) (zip [lineNum..] ls)
        pure $ foldr Map.union Map.empty varSymbols
    processDecl (lineNum, GoConst (ConstDecl ls _)) = do
        constSymbols <- mapM (uncurry processConstantDeclaration) (zip [lineNum..] ls)
        pure $ foldr Map.union Map.empty constSymbols
    processDecl (lineNum, GoType (TypeDecl ls _)) = case ls of
        [] -> pure Map.empty
        (h:_) -> processTypeDeclaration lineNum h
    processDecl _ = pure Map.empty

processLineForSymbols :: (Int, String) -> IntegratedAnalyzer SymbolTable
processLineForSymbols (lineNum, line) = do
    let trimmed = trim line
    if isIgnorable trimmed
        then pure Map.empty
        else dispatch trimmed
  where
    dispatch trimmed =
        if "var " `isPrefixOf` trimmed || ":=" `isInfixOf` trimmed
            then processVariableDeclaration lineNum trimmed
        else if "type " `isPrefixOf` trimmed
            then processTypeDeclaration lineNum trimmed
        else if "func " `isPrefixOf` trimmed
            then processFunctionDeclaration lineNum trimmed
        else if "const " `isPrefixOf` trimmed
            then processConstantDeclaration lineNum trimmed
        else pure Map.empty

    isIgnorable s =
        "//" `isPrefixOf` s
            || null s
            || "/*" `isPrefixOf` s
            || "*/" `isSuffixOf` s

processVariableDeclaration :: Int -> String -> SymbolCollector SymbolTable
processVariableDeclaration lineNum line = do
    let varName = extractVariableNameFromLine line
        varType = extractTypeFromLine line
    if not (null varName) && isValidIdentifier varName
        then do
            symbolInfo <- createSymbolInfo varName varType lineNum
            pure $ Map.singleton varName symbolInfo
        else pure Map.empty

processTypeDeclaration :: Int -> String -> SymbolCollector SymbolTable
processTypeDeclaration lineNum line = do
    let (typeName, typeParams, cs) = parseTypeDeclaration line
    if not (null typeName) && isValidIdentifier typeName
        then do
            typeSymbol <- createTypeSymbolInfo typeName typeParams cs lineNum
            pure $ Map.singleton typeName typeSymbol
        else pure Map.empty

processFunctionDeclaration :: Int -> String -> SymbolCollector SymbolTable
processFunctionDeclaration lineNum line = do
    let funcName = extractFunctionNameFromLine line
    if not (null funcName) && isValidIdentifier funcName
        then do
            symbolInfo <- createFunctionSymbolInfo funcName lineNum
            pure $ Map.singleton funcName symbolInfo
        else pure Map.empty

processConstantDeclaration :: Int -> String -> SymbolCollector SymbolTable
processConstantDeclaration lineNum line = do
    let constName = extractConstantNameFromLine line
    if not (null constName) && isValidIdentifier constName
        then do
            symbolInfo <- createConstantSymbolInfo constName lineNum
            pure $ Map.singleton constName symbolInfo
        else pure Map.empty

typeDeclarationParts :: String -> [String]
typeDeclarationParts = words

isValidIdentifier :: String -> Bool
isValidIdentifier name =
    not (null name)
        && not (isReservedName name)
        && case name of
            [] -> False
            (c : _) -> not (isDigit c) && all isAllowed name
  where
    isAllowed char = isAlphaNum char || char == '_'

createSymbolInfo :: String -> Maybe Dep.TypeVar -> Int -> SymbolCollector SymbolInfo
createSymbolInfo name mType _lineNum = do
    currentScope' <- gets currentScope
    pure
        SymbolInfo
            { symbolName = name
            , symbolType = mType
            , ownershipState = Just $ Own.Owned name
            , symbolScope = currentScope'
            , isMoved = False
            , isBorrowed = False
            , constraints = []
            }

createTypeSymbolInfo :: String -> [String] -> [Dep.Constraint] -> Int -> SymbolCollector SymbolInfo
createTypeSymbolInfo name _params cs _lineNum = do
    currentScope' <- gets currentScope
    pure
        SymbolInfo
            { symbolName = name
            , symbolType = Just $ Dep.TVCon name
            , ownershipState = Nothing
            , symbolScope = currentScope'
            , isMoved = False
            , isBorrowed = False
            , constraints = cs
            }

createFunctionSymbolInfo :: String -> Int -> SymbolCollector SymbolInfo
createFunctionSymbolInfo name _lineNum = do
    currentScope' <- gets currentScope
    pure
        SymbolInfo
            { symbolName = name
            , symbolType = Just (Dep.TVFun [] (Dep.TVCon "void"))
            , ownershipState = Nothing
            , symbolScope = currentScope'
            , isMoved = False
            , isBorrowed = False
            , constraints = []
            }

createConstantSymbolInfo :: String -> Int -> SymbolCollector SymbolInfo
createConstantSymbolInfo name _lineNum = do
    currentScope' <- gets currentScope
    pure
        SymbolInfo
            { symbolName = name
            , symbolType = Just $ Dep.TVCon "const"
            , ownershipState = Just $ Own.Owned name
            , symbolScope = currentScope'
            , isMoved = False
            , isBorrowed = False
            , constraints = []
            }

validateSymbolTable :: SymbolTable -> SymbolCollector SymbolTable
validateSymbolTable symbols = pure $ Map.filterWithKey validateSymbolEntry symbols
  where
    validateSymbolEntry :: String -> SymbolInfo -> Bool
    validateSymbolEntry name symbol =
        isValidIdentifier name
            && not (isReservedName name)
            && isValidSymbolInfo symbol

    isValidSymbolInfo :: SymbolInfo -> Bool
    isValidSymbolInfo symbol =
        not (null $ symbolName symbol)
            && isValidIdentifier (symbolName symbol)
            && symbolScope symbol >= 0

extractTypeEnvironment :: SymbolTable -> Map.Map String Dep.TypeVar
extractTypeEnvironment = Map.mapMaybe symbolType

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

extractVariableNameFromLine :: String -> String
extractVariableNameFromLine line =
    let wordsList = words line
     in if "var" `isPrefixOf` line
            then if length wordsList >= 2 then wordsList !! 1 else ""
            else case break (== ':') line of
                (name, ':' : '=' : _) -> trim name
                _ -> ""

extractTypeFromLine :: String -> Maybe Dep.TypeVar
extractTypeFromLine line =
    let wordsList = words line
     in if "var" `isPrefixOf` line && length wordsList >= 3
            then Just $ Dep.TVCon (wordsList !! 2)
            else Nothing

parseTypeDeclaration :: String -> (String, [String], [Dep.Constraint])
parseTypeDeclaration line =
    let wordsList = typeDeclarationParts line
     in if length wordsList >= 2 && "type" `isPrefixOf` line
            then
                let typeName = wordsList !! 1
                    typeParams = extractTypeParams (drop 2 wordsList)
                    cs = []
                 in (typeName, typeParams, cs)
            else ("", [], [])

extractTypeParams :: [String] -> [String]
extractTypeParams = filter (not . null) . map (takeWhile (/= '>')) . filter (isPrefixOf "<")

extractFunctionNameFromLine :: String -> String
extractFunctionNameFromLine line =
    let parts = words line
     in if length parts >= 2 && parts !! 0 == "func"
            then takeWhile (/= '(') (parts !! 1)
            else ""

extractConstantNameFromLine :: String -> String
extractConstantNameFromLine line =
    let parts = words line
     in if length parts >= 3 && parts !! 0 == "const"
            then parts !! 1
            else ""

isReservedName :: String -> Bool
isReservedName name =
    name
        `elem` [ "fmt"
               , "main"
               , "func"
               , "var"
               , "let"
               , "if"
               , "else"
               , "for"
               , "return"
               , "import"
               , "package"
               , "type"
               , "struct"
               , "interface"
               , "const"
               , "true"
               , "false"
               , "nil"
               , "int"
               , "string"
               , "bool"
               , "float64"
               ]

