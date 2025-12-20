{-# LANGUAGE RecordWildCards #-}

module Analyzer.SymbolTable (
    collectSymbolsAndTypes,
    collectSymbolsFromAST,
    augmentSymbolTableWithLocals,
    trim,
    isReservedName,
    extractTypeEnvironment
) where

import Analyzer.Types
import qualified Dependencies as Dep
import qualified Ownership as Own
import Compiler.GoAst (GoModule(..), GoDecl(..), FuncDecl(..), VarDecl(..), ConstDecl(..), TypeDecl(..), parseGoModule, flattenDeclLines)
import qualified Compiler.GoVarSpec as GoVar
import Compiler.GoParsing (stripLineComment, nestingDelta, splitTopLevel)

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Char (isSpace, isAlphaNum, isDigit, toLower)
import Data.List (dropWhileEnd, isPrefixOf, mapAccumL)
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)

type SymbolTable = Map.Map String SymbolInfo
type SymbolCollector a = IntegratedAnalyzer a

data RawTypeSpec = RawTypeSpec
    { rtsName :: String
    , rtsParams :: [String]
    , rtsConstraints :: [Dep.Constraint]
    , rtsLine :: Maybe Int
    }


combineSymbolTables :: [SymbolTable] -> SymbolTable
combineSymbolTables = foldr Map.union Map.empty

collectSymbolsAndTypes :: String -> IntegratedAnalyzer SymbolTable
collectSymbolsAndTypes code =
    case parseGoModule (lines code) of
        Right goModule -> collectSymbolsFromAST goModule
        Left err -> throwError $ "Go AST parsing failed: " ++ err

collectSymbolsFromAST :: GoModule -> IntegratedAnalyzer SymbolTable
collectSymbolsFromAST GoModule{..} = do
    let annotatedDecls = annotateDecls gmDecls
    symbolTables <- mapM processDecl annotatedDecls
    let combinedSymbols = combineSymbolTables symbolTables
    validateSymbolTable combinedSymbols
  where
    annotateDecls :: [GoDecl] -> [(Int, GoDecl)]
    annotateDecls decls = snd $ mapAccumL step 1 decls
      where
        step lineStart decl =
            let lineCount = max 1 (length (flattenDeclLines decl))
                nextLine = lineStart + lineCount
            in (nextLine, (lineStart, decl))

    processDecl (lineStart, GoFunc (FuncDecl ls)) = case ls of
        [] -> pure Map.empty
        (header:_) -> processFunctionDeclaration lineStart header
    processDecl (lineStart, GoVar varDecl) =
        processVariableDeclaration lineStart varDecl
    processDecl (lineStart, GoConst constDecl) =
        processConstantDeclaration lineStart constDecl
    processDecl (lineStart, GoType typeDecl) =
        processTypeDeclaration lineStart typeDecl
    processDecl _ = pure Map.empty

processVariableDeclaration :: Int -> VarDecl -> SymbolCollector SymbolTable
processVariableDeclaration lineStart varDecl = do
    let specs = GoVar.parseVarDeclRawSpecs (Just lineStart) varDecl
    symbolTables <- mapM (symbolsFromVarSpec lineStart) specs
    pure $ combineSymbolTables symbolTables

processTypeDeclaration :: Int -> TypeDecl -> SymbolCollector SymbolTable
processTypeDeclaration lineStart typeDecl = do
    let specs = parseTypeDeclRawSpecs (Just lineStart) typeDecl
    symbolTables <- mapM (symbolsFromTypeSpec lineStart) specs
    pure $ combineSymbolTables symbolTables

processFunctionDeclaration :: Int -> String -> SymbolCollector SymbolTable
processFunctionDeclaration lineNum line = do
    let funcName = extractFunctionNameFromLine line
    if not (null funcName) && isValidIdentifier funcName
        then do
            symbolInfo <- createFunctionSymbolInfo funcName lineNum
            pure $ Map.singleton funcName symbolInfo
        else pure Map.empty

processConstantDeclaration :: Int -> ConstDecl -> SymbolCollector SymbolTable
processConstantDeclaration lineStart constDecl = do
    let specs = GoVar.parseConstDeclRawSpecs (Just lineStart) constDecl
    symbolTables <- mapM (symbolsFromConstSpec lineStart) specs
    pure $ combineSymbolTables symbolTables

symbolsFromVarSpec :: Int -> GoVar.RawVarSpec -> SymbolCollector SymbolTable
symbolsFromVarSpec fallback GoVar.RawVarSpec{..} = do
    let declLine = fromMaybe fallback rvsLine
        typeVar = convertTypeAnnotation rvsType
    entries <- mapM (varEntry declLine typeVar) rvsNames
    pure $ Map.fromList (catMaybes entries)
  where
    varEntry line typeVar name
        | isValidIdentifier name = do
            info <- createSymbolInfo name typeVar line
            pure (Just (name, info))
        | otherwise = pure Nothing

symbolsFromConstSpec :: Int -> GoVar.RawVarSpec -> SymbolCollector SymbolTable
symbolsFromConstSpec fallback GoVar.RawVarSpec{..} = do
    let declLine = fromMaybe fallback rvsLine
    entries <- mapM (constEntry declLine) rvsNames
    pure $ Map.fromList (catMaybes entries)
  where
    constEntry line name
        | isValidIdentifier name = do
            info <- createConstantSymbolInfo name line
            pure (Just (name, info))
        | otherwise = pure Nothing

symbolsFromTypeSpec :: Int -> RawTypeSpec -> SymbolCollector SymbolTable
symbolsFromTypeSpec fallback RawTypeSpec{..} = do
    let declLine = fromMaybe fallback rtsLine
    if isValidIdentifier rtsName
        then do
            info <- createTypeSymbolInfo rtsName rtsParams rtsConstraints declLine
            pure $ Map.singleton rtsName info
        else pure Map.empty

convertTypeAnnotation :: Maybe String -> Maybe Dep.TypeVar
convertTypeAnnotation Nothing = Nothing
convertTypeAnnotation (Just raw) =
    let normalized = normalizeTypeString raw
    in if null normalized then Nothing else Just (Dep.TVCon normalized)

normalizeTypeString :: String -> String
normalizeTypeString = collapseSpaces . trim
  where
    collapseSpaces [] = []
    collapseSpaces (c:cs)
        | isSpace c = ' ' : collapseSpaces (dropWhile isSpace cs)
        | otherwise = c : collapseSpaces cs

parseTypeDeclRawSpecs :: Maybe Int -> TypeDecl -> [RawTypeSpec]
parseTypeDeclRawSpecs _ TypeDecl{ typeLines = [] } = []
parseTypeDeclRawSpecs start TypeDecl{ typeLines = ls, typeIsGroup = False } =
    maybe [] (:[]) (parseSingleTypeSpec start ls)
parseTypeDeclRawSpecs start TypeDecl{ typeLines = ls, typeIsGroup = True } =
    parseGroupedTypeSpecs start ls

parseSingleTypeSpec :: Maybe Int -> [String] -> Maybe RawTypeSpec
parseSingleTypeSpec start lines0 = do
    (mLine, header) <- firstMeaningfulLine start lines0
    (name, params) <- parseTypeSpecHeader header
    pure RawTypeSpec
        { rtsName = name
        , rtsParams = params
        , rtsConstraints = []
        , rtsLine = mLine
        }

parseGroupedTypeSpecs :: Maybe Int -> [String] -> [RawTypeSpec]
parseGroupedTypeSpecs start lines0 =
    let annotated = annotateWithLines start lines0
        inner = drop 1 (dropWhileEnd isGroupClosing annotated)
    in reverse (collectSpecs inner Nothing "" 0 [])
  where
    isGroupClosing :: (a, String) -> Bool
    isGroupClosing (_, line) = trim (stripLineComment line) == ")"

    collectSpecs [] currentStart current _ acc
        | null (trim current) = acc
        | otherwise =
            case finalizeSpec currentStart current of
                Nothing -> acc
                Just spec -> spec : acc
    collectSpecs ((mLine, raw):rest) currentStart current depth acc =
        let stripped = trim (stripLineComment raw)
        in if null stripped
            then collectSpecs rest currentStart current depth acc
            else
                let nextText = if null current then stripped else current ++ " " ++ stripped
                    startLine = currentStart <|> mLine
                    depth' = depth + nestingDelta stripped
                in if depth' <= 0
                    then collectSpecs rest Nothing "" 0 (maybeAddSpec startLine mLine nextText acc)
                    else collectSpecs rest startLine nextText depth' acc

    maybeAddSpec startLine fallback text acc =
        case finalizeSpec (startLine <|> fallback) text of
            Nothing -> acc
            Just spec -> spec : acc

    finalizeSpec mLine text = do
        (name, params) <- parseTypeSpecHeader text
        pure RawTypeSpec
            { rtsName = name
            , rtsParams = params
            , rtsConstraints = []
            , rtsLine = mLine
            }

firstMeaningfulLine :: Maybe Int -> [String] -> Maybe (Maybe Int, String)
firstMeaningfulLine start lines0 =
    listToMaybe
        [ (lineNo, trim (stripLineComment line))
        | (lineNo, line) <- annotateWithLines start lines0
        , let stripped = trim (stripLineComment line)
        , not (null stripped)
        ]

annotateWithLines :: Maybe Int -> [String] -> [(Maybe Int, String)]
annotateWithLines Nothing ls = [(Nothing, line) | line <- ls]
annotateWithLines (Just start) ls =
    let count = length ls
        lineNumbers = [start .. start + count - 1]
    in zip (map Just lineNumbers) ls

parseTypeSpecHeader :: String -> Maybe (String, [String])
parseTypeSpecHeader raw = do
    let cleaned = trim raw
        withoutKeyword =
            if "type " `isPrefixOf` cleaned
                then dropWhile isSpace (drop (length "type") cleaned)
                else cleaned
    (namePart, rest) <- parseTypeName withoutKeyword
    let (params, _) = parseTypeParams rest
    pure (namePart, params)

parseTypeName :: String -> Maybe (String, String)
parseTypeName text =
    let trimmed = dropWhile isSpace text
        (namePart, rest) = span isTypeNameChar trimmed
    in if null namePart then Nothing else Just (namePart, rest)
  where
    isTypeNameChar c = isAlphaNum c || c == '_'

parseTypeParams :: String -> ([String], String)
parseTypeParams text =
    let trimmed = dropWhile isSpace text
    in case trimmed of
        '[' : _ ->
            case consumeBalanced '[' ']' trimmed of
                Nothing -> ([], trimmed)
                Just (inside, after) ->
                    ( parseParamNames inside
                    , after
                    )
        _ -> ([], trimmed)

parseParamNames :: String -> [String]
parseParamNames inside =
    [ paramName
    | segment <- splitTopLevel ',' inside
    , let cleaned = dropWhile isSpace segment
          paramName = takeWhile isParamChar cleaned
    , not (null paramName)
    ]
  where
    isParamChar c = isAlphaNum c || c == '_' || c == '.'

consumeBalanced :: Char -> Char -> String -> Maybe (String, String)
consumeBalanced open close input =
    case dropWhile isSpace input of
        c:rest | c == open -> go 1 [] rest
        _ -> Nothing
  where
    go :: Int -> String -> String -> Maybe (String, String)
    go _ _ [] = Nothing
    go depth acc (x:xs)
        | x == open = go (depth + 1) (x:acc) xs
        | x == close =
            if depth == 1
                then Just (reverse acc, xs)
                else go (depth - 1) (x:acc) xs
        | otherwise = go depth (x:acc) xs

isValidIdentifier :: String -> Bool
isValidIdentifier name =
    not (null name)
        && not (isReservedName name)
        && case name of
            [] -> False
            (c : _) -> not (isDigit c) && all isAllowed name
  where
    isAllowed char = isAsciiAlphaNum char || char == '_'
    isAsciiAlphaNum char = (char >= 'a' && char <= 'z') || 
                          (char >= 'A' && char <= 'Z') || 
                          (char >= '0' && char <= '9')

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

augmentSymbolTableWithLocals :: String -> SymbolTable -> SymbolTable
augmentSymbolTableWithLocals source initialSymbols =
    snd $ List.foldl' processLine (0, initialSymbols) (lines source)
  where
    processLine (depth, acc) rawLine =
        let depthBefore = max 0 depth
            trimmedLine = trim rawLine
            acc'
              | depthBefore > 0 && isVarDeclLine trimmedLine =
                  addLocalVarSymbols depthBefore trimmedLine acc
              | otherwise = acc
            depthAfterRaw = depthBefore + braceDeltaLine rawLine
            depthAfter = max 0 depthAfterRaw
        in (depthAfter, acc')

    isVarDeclLine txt = "var " `isPrefixOf` txt

    addLocalVarSymbols depthBefore line acc =
        case GoVar.parseVarSpecRaw line of
            Nothing -> acc
            Just spec
              | not (declaresOwned spec) -> acc
              | otherwise ->
                  List.foldl' (insertLocalSymbol depthBefore spec) acc (GoVar.rvsNames spec)

    insertLocalSymbol depthBefore spec acc name
        | Map.member name acc = acc
        | otherwise =
            let typeVar = convertTypeAnnotation (GoVar.rvsType spec)
                info = SymbolInfo
                    { symbolName = name
                    , symbolType = typeVar
                    , ownershipState = Just (Own.Owned name)
                    , symbolScope = depthBefore
                    , isMoved = False
                    , isBorrowed = False
                    , constraints = []
                    }
            in Map.insert name info acc

    declaresOwned spec =
        case GoVar.rvsType spec of
            Just ty ->
                let lowered = map toLower ty
                in "owned" `elem` words lowered
            Nothing -> False

    braceDeltaLine :: String -> Int
    braceDeltaLine = List.foldl' update 0
      where
        update :: Num a => a -> Char -> a
        update acc '{' = acc + 1
        update acc '}' = acc - 1
        update acc _   = acc

extractTypeEnvironment :: SymbolTable -> Map.Map String Dep.TypeVar
extractTypeEnvironment = Map.mapMaybe symbolType

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

extractFunctionNameFromLine :: String -> String
extractFunctionNameFromLine line =
    let parts = words line
     in if length parts >= 2 && parts !! 0 == "func"
            then takeWhile (\c -> c /= '(' && c /= '[') (parts !! 1)
            else ""

isReservedName :: String -> Bool
isReservedName name =
    name
        `elem` [ "fmt"
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

