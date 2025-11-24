{-# LANGUAGE RecordWildCards #-}
module Compiler.TypeChecker (
    Type(..),
    TypeEnv(..),
    TypeCheckDiagnostic(..),
    hasTypeErrors,
    diagnoseTypeErrors,
    extractDeclarations,
    extractFunctionCalls,
    buildTypeEnv,
    isMethodDeclaration,
    checkTypeError,
    hasMalformedSyntax,
    trim
) where

import Parser (TypusFile(..))
import Compiler.Errors (CompilerError)
import Compiler.GoAst
import Compiler.GoParsing (consumeNames, splitTopLevel, stripLineComment)
import qualified Compiler.GoVarSpec as GoVar
import qualified Compiler.IR as IR

import Control.Applicative ((<|>))
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Utils (trim)

-- | Lightweight representation of a type in the simplified checker.
data Type
    = TypeName String
    | UnknownType
    deriving (Eq, Ord, Show)

-- | Function parameter metadata.
data FunctionParam = FunctionParam
    { fpName :: Maybe String
    , fpType :: Type
    , fpVariadic :: Bool
    } deriving (Eq, Show)

-- | Function signature containing positional parameters and return types.
data FunctionSignature = FunctionSignature
    { fsParams :: [FunctionParam]
    , fsReturns :: [Type]
    , fsTypeParams :: [String]
    } deriving (Eq, Show)

-- | Environment containing discovered variable and function types.
data TypeEnv = TypeEnv
    { varTypes :: Map String Type
    , functionTypes :: Map String FunctionSignature
    } deriving (Show)

data VarSpec = VarSpec
    { vsNames :: [String]
    , vsType :: Maybe Type
    , vsValues :: [String]
    } deriving (Show)

data CallExpr = CallExpr
    { callName :: String
    , callArgs :: [String]
    } deriving (Eq, Show)

data TypeError = TypeError
    { teContext :: Maybe String
    , teMessage :: String
    } deriving (Eq, Show)

-- | Public diagnostic representation exposed to the compiler pipeline.
data TypeCheckDiagnostic = TypeCheckDiagnostic
    { tcdContext :: Maybe String
    , tcdMessage :: String
    } deriving (Eq, Show)


-- | Determine whether the given Typus file has malformed Go syntax.
hasMalformedSyntax :: TypusFile -> Bool
hasMalformedSyntax typusFile =
    let source = IR.rawSourceFromTypus typusFile
    in null (trim source) || case parseGoModule (lines source) of
        Left _ -> True
        Right _ -> False

-- | Entry point for the simplified checker.
hasTypeErrors :: TypusFile -> Bool
hasTypeErrors typusFile =
    case diagnoseTypeErrors typusFile of
        Left _ -> True
        Right diagnostics -> not (null diagnostics)

-- | Collect detailed diagnostics for type errors.
diagnoseTypeErrors :: TypusFile -> Either [CompilerError] [TypeCheckDiagnostic]
diagnoseTypeErrors typusFile =
    case IR.moduleFromTypus typusFile of
        Left errs -> Left errs
        Right goModule ->
            let env = buildTypeEnv goModule
                errors = gatherTypeErrors env goModule
            in Right (map toDiagnostic errors)
  where
    toDiagnostic TypeError{..} = TypeCheckDiagnostic
        { tcdContext = teContext
        , tcdMessage = teMessage
        }

-- | Extract top-level declarations (function headers, var/const specs).
extractDeclarations :: String -> [String]
extractDeclarations content =
    case parseGoModule (lines content) of
        Left _ -> []
        Right goModule -> collectDeclStrings goModule
  where
    collectDeclStrings GoModule{..} = concatMap declToStrings gmDecls

    declToStrings (GoFunc (FuncDecl ls)) =
        case ls of
            [] -> []
            (header:_) -> [trim header]
    declToStrings (GoVar (VarDecl ls _)) = map trim (filter (not . null) ls)
    declToStrings (GoConst (ConstDecl ls _)) = map trim (filter (not . null) ls)
    declToStrings _ = []

-- | Extract function call expressions from a Go-like module string.
extractFunctionCalls :: String -> [String]
extractFunctionCalls content =
    case parseGoModule (lines content) of
        Left _ -> []
        Right goModule -> map renderCall (collectModuleCalls goModule)
  where
    renderCall CallExpr{..} =
        let argsText = intercalate ", " (map trim callArgs)
        in callName ++ "(" ++ argsText ++ ")"

-- | Build the type environment using the Go AST.
buildTypeEnv :: GoModule -> TypeEnv
buildTypeEnv GoModule{..} =
    let funcEntries = mapMaybe functionEntry gmDecls
        varEntries = concatMap varEntry gmDecls
    in TypeEnv
        { varTypes = Map.fromList varEntries
        , functionTypes = Map.fromList funcEntries
        }
  where
    functionEntry (GoFunc decl) = do
        info <- parseFunctionInfo decl
        pure (fiName info, fiSignature info)
    functionEntry _ = Nothing

    varEntry (GoVar decl) = extractVarTypes decl
    varEntry (GoConst decl) = extractConstTypes decl
    varEntry _ = []

-- | Legacy line-based API maintained for compatibility.
checkTypeError :: TypeEnv -> String -> Bool
checkTypeError env rawLine =
    let cleaned = trim (stripLineComment rawLine)
    in if null cleaned
        then True
        else
            let errors =
                    if "var " `isPrefixOf` cleaned || "const " `isPrefixOf` cleaned
                        then maybe [] (checkVarSpec env Nothing) (parseVarSpec cleaned)
                    else if '(' `elem` cleaned
                        then concatMap (checkCall env Nothing) (extractCallExpressions cleaned)
                        else []
            in null errors

-- | Identify method declarations based on the function header.
isMethodDeclaration :: String -> Bool
isMethodDeclaration line =
    let trimmed = trim line
    in case dropWhile isSpace (drop (length "func") trimmed) of
        ('(' : _) -> True
        _ -> False

--------------------------------------------------------------------------------
-- Internal analysis
--------------------------------------------------------------------------------

data FunctionInfo = FunctionInfo
    { fiName :: String
    , fiSignature :: FunctionSignature
    , fiBody :: String
    } deriving (Show)

-- | Aggregate all type errors from the module.
gatherTypeErrors :: TypeEnv -> GoModule -> [TypeError]
gatherTypeErrors env GoModule{..} =
    let functionInfos = mapMaybe parseFunctionInfoFromDecl gmDecls
        functionErrors = concatMap (checkFunction env) functionInfos
        statementErrors = concatMap (checkStatement env) [ls | GoStatement (StatementBlock ls) <- gmDecls]
        topVarErrors = concatMap (checkTopLevelVar env) gmDecls
    in functionErrors ++ statementErrors ++ topVarErrors

checkFunction :: TypeEnv -> FunctionInfo -> [TypeError]
checkFunction env FunctionInfo{..} =
    let calls = extractCallExpressions fiBody
    in concatMap (checkCall env (Just fiName)) calls

checkStatement :: TypeEnv -> [String] -> [TypeError]
checkStatement env lines0 =
    let text = unlines lines0
        calls = extractCallExpressions text
    in concatMap (checkCall env Nothing) calls

checkTopLevelVar :: TypeEnv -> GoDecl -> [TypeError]
checkTopLevelVar env decl = case decl of
    GoVar varDecl -> concatMap (checkVarSpec env Nothing) (parseVarDeclSpecs varDecl)
    GoConst constDecl -> concatMap (checkVarSpec env Nothing) (parseConstDeclSpecs constDecl)
    _ -> []

checkVarSpec :: TypeEnv -> Maybe String -> VarSpec -> [TypeError]
checkVarSpec env context VarSpec{..} =
    case vsType of
        Nothing -> []
        Just declaredType ->
            let inferred = map (inferArgumentType env) vsValues
                pairs = zip vsNames inferred
            in if length vsValues == length vsNames
                then
                    [ TypeError context ("Variable '" ++ name ++ "' expects type " ++ showType declaredType ++
                        ", but expression has type " ++ showType actual)
                    | (name, actual) <- pairs
                    , not (typesCompatible declaredType actual)
                    , actual /= UnknownType
                    ]
                else []

checkCall :: TypeEnv -> Maybe String -> CallExpr -> [TypeError]
checkCall TypeEnv{..} context CallExpr{..} =
    case lookupSignature callName of
        Nothing -> []
        Just signature ->
            let arityErrors = checkArity signature
                typeErrors = checkArgumentTypes signature
            in arityErrors ++ typeErrors
  where
    lookupSignature name =
        Map.lookup name functionTypes <|> Map.lookup (lastSegment name) functionTypes

    lastSegment n =
        case break (== '.') (reverse n) of
            (revSuffix, []) -> reverse revSuffix
            (_, _:revPrefix) -> reverse revPrefix

    checkArity FunctionSignature{..} =
        let params = fsParams
            (fixedParams, variadicParam) =
                case params of
                    [] -> ([], Nothing)
                    _ ->
                        let lastParam = last params
                        in if fpVariadic lastParam
                              then (init params, Just lastParam)
                              else (params, Nothing)
            minCount = length fixedParams
            actualCount = length callArgs
            tooFew = actualCount < minCount
            tooMany = variadicParam == Nothing && actualCount > minCount
            buildMsg msg = [TypeError context (callName ++ ": " ++ msg)]
        in concat
            [ if tooFew
                then buildMsg ("expected at least " ++ show minCount ++ " arguments, got " ++ show actualCount)
                else []
            , if tooMany
                then buildMsg ("expected " ++ show minCount ++ " arguments, got " ++ show actualCount)
                else []
            ]

    checkArgumentTypes signature =
        let params = fsParams signature
            (fixedParams, variadicParam) =
                case params of
                    [] -> ([], Nothing)
                    _ ->
                        let lp = last params
                        in if fpVariadic lp then (init params, Just lp) else (params, Nothing)
            expectedForIdx idx
                | idx < length fixedParams = Just (fpType (fixedParams !! idx))
                | otherwise = fpType <$> variadicParam
            indexedArgs :: [(Int, String)]
            indexedArgs = zip [0..] callArgs
        in concatMap (checkArg (fsTypeParams signature) expectedForIdx) indexedArgs

    checkArg typeParams expected (idx, argText) =
        case expected idx of
            Nothing -> []
            Just expectedType ->
                let actualType = inferArgumentType (TypeEnv varTypes functionTypes) argText
                    matchesGeneric = expectedMatchesGeneric typeParams expectedType
                    matchesConcrete = typesCompatible expectedType actualType || actualType == UnknownType
                in if matchesGeneric || matchesConcrete
                      then []
                      else [TypeError context (callName ++ " argument " ++ show (idx + 1) ++
                                ": expected type " ++ showType expectedType ++
                                ", got " ++ showType actualType)]

--------------------------------------------------------------------------------
-- Parsing helpers
--------------------------------------------------------------------------------

parseFunctionInfoFromDecl :: GoDecl -> Maybe FunctionInfo
parseFunctionInfoFromDecl (GoFunc decl) = parseFunctionInfo decl
parseFunctionInfoFromDecl _ = Nothing

parseFunctionInfo :: FuncDecl -> Maybe FunctionInfo
parseFunctionInfo (FuncDecl []) = Nothing
parseFunctionInfo (FuncDecl (header:bodyLines))
    | isMethodDeclaration header = Nothing
    | otherwise = do
        signature <- parseFunctionSignature header
        functionName <- extractFunctionName header
        let bodyText = unlines (dropClosingBrace bodyLines)
        pure FunctionInfo
            { fiName = functionName
            , fiSignature = signature
            , fiBody = bodyText
            }
  where
    dropClosingBrace [] = []
    dropClosingBrace ls =
        let trimmed = trim (last ls)
        in if trimmed == "}"
              then init ls
              else ls

extractFunctionName :: String -> Maybe String
extractFunctionName header = do
    rest <- stripPrefix "func" (trim header)
    let after = dropWhile isSpace rest
    guard (case after of
        [] -> False
        '(' : _ -> False
        _ -> True)
    let namePart = takeWhile isValid after
    guard (not (null namePart))
    pure namePart
  where
    guard True = Just ()
    guard False = Nothing
    isValid c = isAlphaNum c || c == '_'

parseFunctionSignature :: String -> Maybe FunctionSignature
parseFunctionSignature rawHeader = do
    (_, afterFunc) <- stripPrefixWith "func" (trim rawHeader)
    let afterTrim = dropWhile isSpace afterFunc
    guard (case afterTrim of
        [] -> False
        '(' : _ -> False
        _ -> True)
    nameAndRest <- pure afterTrim
    let (namePart, rest0) = break (`elem` "([") nameAndRest
    guard (not (null namePart))
    let inlineTypeParams = parseLegacyTypeParams namePart
        rest1 = dropWhile isSpace rest0
    (bracketTypeParams, afterGenericsSource) <-
        case rest1 of
            '[' : _ -> do
                (rawParams, afterBracket) <- consumeBalanced '[' ']' rest1
                pure (parseBracketTypeParams rawParams, afterBracket)
            _ -> pure ([], rest1)
    let typeParams = if null bracketTypeParams then inlineTypeParams else bracketTypeParams
    (_, paramsSection, afterParams) <- consumeParenSection afterGenericsSource
    let paramSegments = splitTopLevel ',' paramsSection
        params = concatMap parseParamSegment paramSegments
        returns = parseReturnTypes (dropWhile isSpace afterParams)
    pure FunctionSignature
        { fsParams = params
        , fsReturns = returns
        , fsTypeParams = typeParams
        }
  where
    stripPrefixWith prefix s
        | prefix `isPrefixOf` s = Just (prefix, drop (length prefix) s)
        | otherwise = Nothing

    guard True = Just ()
    guard False = Nothing

    parseLegacyTypeParams :: String -> [String]
    parseLegacyTypeParams text =
        case break (== '<') text of
            (_, []) -> []
            (_, '<':rest) ->
                case span (/= '>') rest of
                    (inner, '>':_) -> parseTypeParamNames inner
                    _ -> []
            _ -> []

    parseBracketTypeParams :: String -> [String]
    parseBracketTypeParams = parseTypeParamNames

    parseTypeParamNames :: String -> [String]
    parseTypeParamNames raw =
        [ name
        | segment <- splitTopLevel ',' raw
        , let name = takeParamName segment
        , not (null name)
        ]

    takeParamName :: String -> String
    takeParamName segment =
        let cleaned = trim segment
        in takeWhile isParamNameChar cleaned

    isParamNameChar :: Char -> Bool
    isParamNameChar c = isAlphaNum c || c == '_'

parseParamSegment :: String -> [FunctionParam]
parseParamSegment rawSegment =
    let segment = trim rawSegment
    in if null segment
        then []
        else
            case consumeNames segment of
                ([], _) -> [FunctionParam Nothing (typeFromString segment) False]
                (names, remainder) ->
                    let (isVariadic, typeStr) = stripVariadic remainder
                        paramType = typeFromString typeStr
                    in [ FunctionParam (Just name) paramType isVariadic | name <- names ]

parseReturnTypes :: String -> [Type]
parseReturnTypes raw =
    let trimmed = trim raw
    in if null trimmed
        then []
        else if startsWithChar '(' trimmed && endsWithChar ')' trimmed
            then case stripOuterParens trimmed of
                Nothing -> [typeFromString trimmed]
                Just inner -> map (typeFromString . extractTypeComponent) (splitTopLevel ',' inner)
            else [typeFromString trimmed]

extractTypeComponent :: String -> String
extractTypeComponent segment =
    let s = trim segment
        (names, remainder) = consumeNames s
    in if null names then s else trim remainder

parseVarDeclSpecs :: VarDecl -> [VarSpec]
parseVarDeclSpecs decl = map toVarSpec (GoVar.parseVarDeclRawSpecs Nothing decl)

parseConstDeclSpecs :: ConstDecl -> [VarSpec]
parseConstDeclSpecs decl = map toVarSpec (GoVar.parseConstDeclRawSpecs Nothing decl)

toVarSpec :: GoVar.RawVarSpec -> VarSpec
toVarSpec GoVar.RawVarSpec{..} =
    VarSpec
        { vsNames = rvsNames
        , vsType = fmap typeFromString rvsType
        , vsValues = rvsValues
        }

parseVarSpec :: String -> Maybe VarSpec
parseVarSpec raw = toVarSpec <$> GoVar.parseVarSpecRaw raw

extractConstTypes :: ConstDecl -> [(String, Type)]
extractConstTypes decl =
    [ (name, ty)
    | VarSpec{..} <- parseConstDeclSpecs decl
    , Just ty <- [vsType]
    , name <- vsNames
    ]

extractVarTypes :: VarDecl -> [(String, Type)]
extractVarTypes decl =
    [ (name, ty)
    | VarSpec{..} <- parseVarDeclSpecs decl
    , Just ty <- [vsType]
    , name <- vsNames
    ]

--------------------------------------------------------------------------------
-- Low level parsing utilities
--------------------------------------------------------------------------------

typeFromString :: String -> Type
typeFromString s =
    let normal = normalizeTypeName s
    in if null normal then UnknownType else TypeName normal

normalizeTypeName :: String -> String
normalizeTypeName = collapseSpaces . trim
  where
    collapseSpaces [] = []
    collapseSpaces (c:cs)
        | isSpace c = ' ' : collapseSpaces (dropWhile isSpace cs)
        | otherwise = c : collapseSpaces cs


stripVariadic :: String -> (Bool, String)
stripVariadic raw =
    let t = trim raw
    in if "..." `isPrefixOf` t
        then (True, trim (drop 3 t))
        else (False, t)



consumeBalanced :: Char -> Char -> String -> Maybe (String, String)
consumeBalanced open close input =
    case dropWhile isSpace input of
        c:rest | c == open -> go 1 [] rest
        _ -> Nothing
  where
    go :: Int -> String -> String -> Maybe (String, String)
    go _ _ [] = Nothing
    go level acc (x:xs)
        | x == open = go (level + 1) (x:acc) xs
        | x == close =
            if level == 1
                then Just (reverse acc, xs)
                else go (level - 1) (x:acc) xs
        | otherwise = go level (x:acc) xs

consumeParenSection :: String -> Maybe (String, String, String)
consumeParenSection text =
    case dropWhile isSpace text of
        '(' : rest -> go 1 [] rest
        _ -> Nothing
  where
    go :: Int -> String -> String -> Maybe (String, String, String)
    go _ _ [] = Nothing
    go level acc (c:cs)
        | c == '(' = go (level + 1) (c:acc) cs
        | c == ')' && level == 1 =
            let content = reverse acc
            in Just ("(", content, cs)
        | c == ')' = go (level - 1) (c:acc) cs
        | otherwise = go level (c:acc) cs

stripOuterParens :: String -> Maybe String
stripOuterParens s =
    case dropWhile isSpace s of
        '(' : rest -> reverseStrip rest [] 1
        _ -> Nothing
  where
    reverseStrip :: String -> String -> Int -> Maybe String
    reverseStrip [] _ _ = Nothing
    reverseStrip (c:cs) acc depth
        | c == '(' = reverseStrip cs (c:acc) (depth + 1)
        | c == ')' && depth == 1 = Just (reverse acc)
        | c == ')' = reverseStrip cs (c:acc) (depth - 1)
        | otherwise = reverseStrip cs (c:acc) depth



--------------------------------------------------------------------------------
-- Call extraction
--------------------------------------------------------------------------------

data ReaderState
    = NoStringState'
    | DoubleState Bool
    | SingleState Bool
    | BacktickState'
    deriving (Eq)

extractCallExpressions :: String -> [CallExpr]
extractCallExpressions input = go 0 NoStringState' 0 []
  where
    len = length input

    go :: Int -> ReaderState -> Int -> [CallExpr] -> [CallExpr]
    go idx state depth acc
        | idx >= len = reverse acc
        | otherwise =
            let ch = input !! idx
            in case state of
                NoStringState' ->
                    case ch of
                        '"' -> go (idx + 1) (DoubleState False) depth acc
                        '\'' -> go (idx + 1) (SingleState False) depth acc
                        '`' -> go (idx + 1) BacktickState' depth acc
                        '(' ->
                            if depth == 0
                                then case collectCall idx of
                                    Nothing -> go (idx + 1) NoStringState' (depth + 1) acc
                                    Just (callExpr, nextIdx) -> go (nextIdx + 1) NoStringState' 0 (callExpr : acc)
                                else go (idx + 1) NoStringState' (depth + 1) acc
                        ')' -> go (idx + 1) NoStringState' (max 0 (depth - 1)) acc
                        _ -> go (idx + 1) NoStringState' depth acc
                DoubleState esc ->
                    let esc' = (not esc && ch == '\\')
                        nextState = if not esc && ch == '"' then NoStringState' else DoubleState esc'
                    in go (idx + 1) nextState depth acc
                SingleState esc ->
                    let esc' = (not esc && ch == '\\')
                        nextState = if not esc && ch == '\'' then NoStringState' else SingleState esc'
                    in go (idx + 1) nextState depth acc
                BacktickState' ->
                    let nextState = if ch == '`' then NoStringState' else BacktickState'
                    in go (idx + 1) nextState depth acc

    collectCall openIdx = do
        name <- collectCallableName (openIdx - 1)
        (argsText, closeIdx) <- collectArgs (openIdx + 1) 1 NoStringState' []
        let args = map trim (splitTopLevel ',' argsText)
        pure (CallExpr name args, closeIdx)

    collectCallableName startIdx
        | startIdx < 0 = Nothing
        | otherwise =
            let j = skipSpaces startIdx
            in if j < 0 then Nothing else extractName j j
      where
        skipSpaces j
            | j < 0 = j
            | isSpace (input !! j) = skipSpaces (j - 1)
            | otherwise = j

        extractName endIdx currentIdx
            | currentIdx < 0 = takeName 0 endIdx
            | otherwise =
                case input !! currentIdx of
                    c | c == ']' ->
                            case findMatching '[' currentIdx of
                                Nothing -> Nothing
                                Just start -> extractName (endIdx) (start - 1)
                      | isValidNameChar c -> extractName endIdx (currentIdx - 1)
                      | isSpace c -> extractName endIdx (currentIdx - 1)
                      | otherwise -> takeName (currentIdx + 1) endIdx

        takeName start end
            | start > end = Nothing
            | otherwise =
                let name = trim (slice start end)
                in if null name || name `elem` keywords
                      then Nothing
                      else Just name

        slice start end = take (end - start + 1) (drop start input)

        keywords = ["if", "for", "switch", "return", "func", "type", "var", "const", "go", "defer"]

        isValidNameChar c = isAlphaNum c || c == '_' || c == '.'

        findMatching _ idx | idx < 0 = Nothing
        findMatching openChar idx = goMatch idx 0
          where
            goMatch :: Int -> Int -> Maybe Int
            goMatch j level
                | j < 0 = Nothing
                | otherwise =
                    let ch = input !! j
                    in if ch == openChar && level == 0
                        then Just j
                        else if ch == openChar
                            then goMatch (j - 1) (level - 1)
                            else if ch == ']'
                                then goMatch (j - 1) (level + 1)
                                else goMatch (j - 1) level

    collectArgs :: Int -> Int -> ReaderState -> String -> Maybe (String, Int)
    collectArgs idx depth state acc
        | idx >= len = Nothing
        | otherwise =
            let ch = input !! idx
            in case state of
                NoStringState' ->
                    case ch of
                        '"' -> collectArgs (idx + 1) depth (DoubleState False) (ch:acc)
                        '\'' -> collectArgs (idx + 1) depth (SingleState False) (ch:acc)
                        '`' -> collectArgs (idx + 1) depth BacktickState' (ch:acc)
                        '(' -> collectArgs (idx + 1) (depth + 1) state (ch:acc)
                        ')' ->
                            if depth == 1
                                then Just (reverse acc, idx)
                                else collectArgs (idx + 1) (depth - 1) state (ch:acc)
                        _ -> collectArgs (idx + 1) depth state (ch:acc)
                DoubleState esc ->
                    let esc' = (not esc && ch == '\\')
                        nextState = if not esc && ch == '"' then NoStringState' else DoubleState esc'
                    in collectArgs (idx + 1) depth nextState (ch:acc)
                SingleState esc ->
                    let esc' = (not esc && ch == '\\')
                        nextState = if not esc && ch == '\'' then NoStringState' else SingleState esc'
                    in collectArgs (idx + 1) depth nextState (ch:acc)
                BacktickState' ->
                    let nextState = if ch == '`' then NoStringState' else BacktickState'
                    in collectArgs (idx + 1) depth nextState (ch:acc)

collectModuleCalls :: GoModule -> [CallExpr]
collectModuleCalls GoModule{..} =
    concatMap gather gmDecls
  where
    gather (GoFunc decl) =
        case parseFunctionInfo decl of
            Nothing -> []
            Just FunctionInfo{..} -> extractCallExpressions fiBody
    gather (GoStatement (StatementBlock ls)) = extractCallExpressions (unlines ls)
    gather _ = []

--------------------------------------------------------------------------------
-- Type inference helpers
--------------------------------------------------------------------------------

inferArgumentType :: TypeEnv -> String -> Type
inferArgumentType TypeEnv{..} rawExpr =
    let expr = trim rawExpr
    in case Map.lookup expr varTypes of
        Just ty -> ty
        Nothing -> inferLiteral expr
  where
    inferLiteral e
        | null e = UnknownType
        | isStringLiteral e = TypeName "string"
        | isRuneLiteral e = TypeName "rune"
        | isBoolLiteral e = TypeName "bool"
        | isNumericLiteral e = numericType e
        | isCompositeLiteral e = TypeName (normalizeTypeName (takeWhile (/= '{') e))
        | otherwise =
            case parseAsCall e of
                Just call ->
                    case inferCallResult call of
                        Just ty -> ty
                        Nothing -> UnknownType
                Nothing -> UnknownType

    parseAsCall expr =
        case extractCallExpressions expr of
            [call] | isWholeCall expr call -> Just call
            _ -> Nothing

    isWholeCall txt CallExpr{..} =
        let trimmedTxt = trim txt
            (namePrefix, rest) = splitAt (length callName) trimmedTxt
            restAfterName = dropWhile isSpace rest
        in namePrefix == callName && case consumeParenSection restAfterName of
            Just (_, _, remainder) -> null (trim remainder)
            Nothing -> False

    inferCallResult CallExpr{..} = do
        signature <- Map.lookup callName functionTypes <|> Map.lookup (lastSegment callName) functionTypes
        case fsReturns signature of
            (ty:_) -> Just ty
            _ -> Nothing

    lastSegment n =
        case break (== '.') (reverse n) of
            (revSuffix, []) -> reverse revSuffix
            (_, _:revPrefix) -> reverse revPrefix

startsWithChar :: Char -> String -> Bool
startsWithChar _ [] = False
startsWithChar c (x:_) = c == x

endsWithChar :: Char -> String -> Bool
endsWithChar _ [] = False
endsWithChar c [x] = x == c
endsWithChar c (_:xs) = endsWithChar c xs

isStringLiteral :: String -> Bool
isStringLiteral s =
    (startsWithChar '"' s && endsWithChar '"' s) || (startsWithChar '`' s && endsWithChar '`' s)

isRuneLiteral :: String -> Bool
isRuneLiteral s = length s >= 2 && startsWithChar '\'' s && endsWithChar '\'' s

isBoolLiteral :: String -> Bool
isBoolLiteral s = s == "true" || s == "false"

isNumericLiteral :: String -> Bool
isNumericLiteral [] = False
isNumericLiteral (c:cs)
    | c == '-' || c == '+' = all isNumericChar cs
    | otherwise = all isNumericChar (c:cs)
  where
    isNumericChar ch = isDigit ch || ch `elem` ['.', '_']

numericType :: String -> Type
numericType s = if '.' `elem` s then TypeName "float64" else TypeName "int"

isCompositeLiteral :: String -> Bool
isCompositeLiteral s = '{' `elem` s && not (null (takeWhile (/= '{') s))

--------------------------------------------------------------------------------
-- Equality helpers
--------------------------------------------------------------------------------

typesCompatible :: Type -> Type -> Bool
typesCompatible UnknownType _ = True
typesCompatible _ UnknownType = True
typesCompatible (TypeName a) (TypeName b) = normalizeTypeName a == normalizeTypeName b

expectedMatchesGeneric :: [String] -> Type -> Bool
expectedMatchesGeneric _ UnknownType = False
expectedMatchesGeneric params (TypeName name) =
    let tokens = splitTypeTokens (normalizeTypeName name)
    in any (`elem` tokens) params

splitTypeTokens :: String -> [String]
splitTypeTokens = go [] []
  where
    go acc current [] =
        let acc' = if null current then acc else reverse current : acc
        in reverse acc'
    go acc current (c:cs)
        | isIdentChar c = go acc (c:current) cs
        | null current = go acc [] cs
        | otherwise = go (reverse current : acc) [] cs

    isIdentChar ch = isAlphaNum ch || ch == '_'

showType :: Type -> String
showType (TypeName n) = n
showType UnknownType = "unknown"


