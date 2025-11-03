{-# LANGUAGE RecordWildCards #-}
module Compiler.TypeChecker (
    Type(..),
    TypeEnv(..),
    hasTypeErrors,
    extractDeclarations,
    extractFunctionCalls,
    buildTypeEnv,
    isMethodDeclaration,
    checkTypeError,
    hasMalformedSyntax,
    trim
) where

import Parser (TypusFile(..))
import Compiler.GoAst
import qualified Compiler.IR as IR

import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.List (dropWhileEnd, foldl', intercalate, isPrefixOf, stripPrefix)
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
    case IR.moduleFromTypus typusFile of
        Left _ -> True
        Right goModule ->
            let env = buildTypeEnv goModule
                errors = gatherTypeErrors env goModule
            in not (null errors)

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
    varEntry (GoConst decl) = extractVarTypes decl
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
checkCall TypeEnv{..} context call@CallExpr{..} =
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
            (revSuffix, _:revPrefix) -> reverse revPrefix

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

    checkArgumentTypes FunctionSignature{..} =
        let params = fsParams
            (fixedParams, variadicParam) =
                case params of
                    [] -> ([], Nothing)
                    _ ->
                        let lp = last params
                        in if fpVariadic lp then (init params, Just lp) else (params, Nothing)
            expectedForIdx idx
                | idx < length fixedParams = Just (fpType (fixedParams !! idx))
                | otherwise = fpType <$> variadicParam
            indexedArgs = zip [0..] callArgs
        in concatMap (checkArg expectedForIdx) indexedArgs

    checkArg expected (idx, argText) =
        case expected idx of
            Nothing -> []
            Just expectedType ->
                let actualType = inferArgumentType (TypeEnv varTypes functionTypes) argText
                in if typesCompatible expectedType actualType || actualType == UnknownType
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
    guard (not (null after) && head after /= '(')
    let namePart = takeWhile isValid after
    guard (not (null namePart))
    pure namePart
  where
    guard True = Just ()
    guard False = Nothing
    isValid c = isAlphaNum c || c == '_'

parseFunctionSignature :: String -> Maybe FunctionSignature
parseFunctionSignature rawHeader = do
    (_, afterFunc) <- stripPrefix "func" (trim rawHeader)
    let afterTrim = dropWhile isSpace afterFunc
    guard (not (null afterTrim) && head afterTrim /= '(')
    nameAndRest <- pure afterTrim
    let (namePart, rest0) = break (`elem` "([") nameAndRest
    guard (not (null namePart))
    let rest1 = dropWhile isSpace rest0
    afterGenericsSource <-
        if not (null rest1) && head rest1 == '['
            then fmap snd (consumeBalanced '[' ']' rest1)
            else Just rest1
    (_, paramsSection, afterParams) <- consumeParenSection afterGenericsSource
    let paramSegments = splitTopLevel ',' paramsSection
        params = concatMap parseParamSegment paramSegments
        returns = parseReturnTypes (dropWhile isSpace afterParams)
    pure FunctionSignature
        { fsParams = params
        , fsReturns = returns
        }
  where
    stripPrefix prefix s
        | prefix `isPrefixOf` s = Just (prefix, drop (length prefix) s)
        | otherwise = Nothing

    guard True = Just ()
    guard False = Nothing

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
        else if head trimmed == '(' && last trimmed == ')'
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
parseVarDeclSpecs VarDecl{..}
    | null varLines = []
    | varIsGroup = parseGroupedSpecs varLines
    | otherwise = maybeToList (parseVarSpec (intercalate " " (map trim varLines)))

parseConstDeclSpecs :: ConstDecl -> [VarSpec]
parseConstDeclSpecs ConstDecl{..}
    | null constLines = []
    | constIsGroup = parseGroupedSpecs constLines
    | otherwise = maybeToList (parseVarSpec (intercalate " " (map trim constLines)))

parseGroupedSpecs :: [String] -> [VarSpec]
parseGroupedSpecs lines0 =
    let inner = drop 1 (dropWhileEnd (\line -> trim line == ")") lines0)
        go [] current depth acc =
            let acc' = if null (trim current) then acc else current : acc
            in reverse acc'
        go (ln:rest) current depth acc =
            let stripped = trim (stripLineComment ln)
                nextCurrent = if null current then stripped else current ++ " " ++ stripped
                depthDelta = nestingDelta stripped
                newDepth = depth + depthDelta
            in if newDepth <= 0
                then go rest "" 0 (if null (trim nextCurrent) then acc else nextCurrent : acc)
                else go rest nextCurrent newDepth acc
        specs = go inner "" 0 []
    in mapMaybe parseVarSpec specs

parseVarSpec :: String -> Maybe VarSpec
parseVarSpec rawLine =
    let line = trim (removeTrailingComma (stripLineComment rawLine))
        withoutKeyword
            | "var " `isPrefixOf` line = drop 4 line
            | "const " `isPrefixOf` line = drop 6 line
            | otherwise = line
    in if null withoutKeyword
        then Nothing
        else
            case findAssignmentIndex withoutKeyword of
                Nothing -> do
                    let (names, remainder) = consumeNames withoutKeyword
                        typePart = trim remainder
                    guard (not (null names))
                    let mType = if null typePart then Nothing else Just (typeFromString typePart)
                    pure VarSpec
                        { vsNames = names
                        , vsType = mType
                        , vsValues = []
                        }
                Just idx -> do
                    let (lhs, rhsRaw) = splitAt idx withoutKeyword
                        rhs = trim (drop 1 rhsRaw)
                        (names, remainder) = consumeNames lhs
                        typePart = trim remainder
                        values = map trim (splitTopLevel ',' rhs)
                    guard (not (null names))
                    let mType = if null typePart then Nothing else Just (typeFromString typePart)
                    pure VarSpec
                        { vsNames = names
                        , vsType = mType
                        , vsValues = values
                        }
  where
    guard True = Just ()
    guard False = Nothing

removeTrailingComma :: String -> String
removeTrailingComma = dropWhileEnd (== ',')

findAssignmentIndex :: String -> Maybe Int
findAssignmentIndex s = go 0
  where
    go idx
        | idx >= length s = Nothing
        | otherwise =
            let c = s !! idx
                prev = if idx == 0 then ' ' else s !! (idx - 1)
                next = if idx + 1 < length s then s !! (idx + 1) else ' '
            in if c == '=' && next /= '=' && prev `notElem` "=<>!"
                  then Just idx
                  else go (idx + 1)

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

consumeNames :: String -> ([String], String)
consumeNames s =
    case parseIdentifier s of
        Nothing -> ([], s)
        Just (ident, rest) ->
            let rest' = dropWhile isSpace rest
            in if null rest'
                then ([], s)
                else gather rest' [ident]
  where
    gather text acc =
        let text' = dropWhile isSpace text
        in case text' of
            [] -> (acc, text')
            ',' : more ->
                case parseIdentifier more of
                    Nothing -> (acc, more)
                    Just (nextName, afterNext) -> gather afterNext (acc ++ [nextName])
            _ -> (acc, text')

stripVariadic :: String -> (Bool, String)
stripVariadic raw =
    let t = trim raw
    in if "..." `isPrefixOf` t
        then (True, trim (drop 3 t))
        else (False, t)

parseIdentifier :: String -> Maybe (String, String)
parseIdentifier [] = Nothing
parseIdentifier (c:cs)
    | isIdentStart c =
        let (restIdent, rest) = span isIdentChar cs
        in Just (c : restIdent, rest)
    | otherwise = Nothing
  where
    isIdentStart ch = isAlphaNum ch || ch == '_' || ch == '.'
    isIdentChar ch = isAlphaNum ch || ch == '_' || ch == '.'

stripLineComment :: String -> String
stripLineComment = go NoString False
  where
    data StrState = NoString | DoubleStr | SingleStr | BacktickStr deriving Eq

    go _ _ [] = []
    go state escaped (x:y:rest)
        | state == NoString && x == '/' && y == '/' = []
        | otherwise = x : goState state escaped y rest
    go state escaped [x] = [x]

    goState state escaped current rest =
        case state of
            NoString ->
                case current of
                    '"' -> '"' : go DoubleStr False rest
                    '\'' -> '\'' : go SingleStr False rest
                    '`' -> '`' : go BacktickStr False rest
                    _ -> current : go NoString False rest
            DoubleStr ->
                let escaped' = if escaped then False else current == '\\'
                    nextState = if not escaped && current == '"' then NoString else DoubleStr
                in current : go nextState escaped' rest
            SingleStr ->
                let escaped' = if escaped then False else current == '\\'
                    nextState = if not escaped && current == '\'' then NoString else SingleStr
                in current : go nextState escaped' rest
            BacktickStr ->
                let nextState = if current == '`' then NoString else BacktickStr
                in current : go nextState False rest

consumeBalanced :: Char -> Char -> String -> Maybe (String, String)
consumeBalanced open close input =
    case dropWhile isSpace input of
        c:rest | c == open -> go 1 [] rest
        _ -> Nothing
  where
    go _ acc [] = Nothing
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
    reverseStrip [] _ _ = Nothing
    reverseStrip (c:cs) acc depth
        | c == '(' = reverseStrip cs (c:acc) (depth + 1)
        | c == ')' && depth == 1 = Just (reverse acc)
        | c == ')' = reverseStrip cs (c:acc) (depth - 1)
        | otherwise = reverseStrip cs (c:acc) depth

splitTopLevel :: Char -> String -> [String]
splitTopLevel delim input = reverse (finalise current acc)
  where
    (acc, current, _) = foldl' step ([], [], NoStringState) input

    finalise cur acc' =
        let piece = trim (reverse cur)
        in if null piece then acc' else piece : acc'

    step (pieces, cur, state) ch =
        case updateState state ch of
            (newState, Just action)
                | action == SplitHere && nullInners newState ->
                    (trim (reverse cur) : pieces, [], newState)
            (newState, _) -> (pieces, ch : cur, newState)

    nullInners s = case s of
        NoStringState -> True
        _ -> False

    updateState st ch = case st of
        NoStringState ->
            case ch of
                '"' -> (DoubleStringState False, Nothing)
                '\'' -> (SingleStringState False, Nothing)
                '`' -> (BacktickStringState, Nothing)
                '(' -> (ParenState 1, Nothing)
                '{' -> (BraceState 1, Nothing)
                '[' -> (BracketState 1, Nothing)
                _
                    | ch == delim -> (NoStringState, Just SplitHere)
                    | otherwise -> (NoStringState, Nothing)
        DoubleStringState escaped ->
            let escaped' = (not escaped && ch == '\\')
                nextState = if not escaped && ch == '"' then NoStringState else DoubleStringState escaped'
            in (nextState, Nothing)
        SingleStringState escaped ->
            let escaped' = (not escaped && ch == '\\')
                nextState = if not escaped && ch == '\'' then NoStringState else SingleStringState escaped'
            in (nextState, Nothing)
        BacktickStringState ->
            let nextState = if ch == '`' then NoStringState else BacktickStringState
            in (nextState, Nothing)
        ParenState depth ->
            let depth' = case ch of
                    '(' -> depth + 1
                    ')' -> depth - 1
                    _ -> depth
                nextState = if depth' == 0 then NoStringState else ParenState depth'
            in (nextState, Nothing)
        BraceState depth ->
            let depth' = case ch of
                    '{' -> depth + 1
                    '}' -> depth - 1
                    _ -> depth
                nextState = if depth' == 0 then NoStringState else BraceState depth'
            in (nextState, Nothing)
        BracketState depth ->
            let depth' = case ch of
                    '[' -> depth + 1
                    ']' -> depth - 1
                    _ -> depth
                nextState = if depth' == 0 then NoStringState else BracketState depth'
            in (nextState, Nothing)

    data SplitState
        = NoStringState
        | DoubleStringState Bool
        | SingleStringState Bool
        | BacktickStringState
        | ParenState Int
        | BraceState Int
        | BracketState Int
        deriving (Eq)

    data SplitAction = SplitHere

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

nestingDelta :: String -> Int
nestingDelta = foldl' step 0
  where
    step acc c = acc + delta c
    delta '(' = 1
    delta ')' = -1
    delta '[' = 1
    delta ']' = -1
    delta '{' = 1
    delta '}' = -1
    delta _ = 0

--------------------------------------------------------------------------------
-- Call extraction
--------------------------------------------------------------------------------

extractCallExpressions :: String -> [CallExpr]
extractCallExpressions input = go 0 NoString False 0 []
  where
    len = length input

    go idx state escaped depth acc
        | idx >= len = reverse acc
        | otherwise =
            let ch = input !! idx
            in case state of
                NoStringState' ->
                    case ch of
                        '"' -> go (idx + 1) (DoubleState False) False depth acc
                        '\'' -> go (idx + 1) (SingleState False) False depth acc
                        '`' -> go (idx + 1) BacktickState' False depth acc
                        '(' ->
                            if depth == 0
                                then case collectCall idx of
                                    Nothing -> go (idx + 1) NoStringState' False (depth + 1) acc
                                    Just (callExpr, nextIdx) -> go (nextIdx + 1) NoStringState' False 0 (callExpr : acc)
                                else go (idx + 1) NoStringState' False (depth + 1) acc
                        ')' -> go (idx + 1) NoStringState' False (max 0 (depth - 1)) acc
                        _ -> go (idx + 1) NoStringState' False depth acc
                DoubleState esc ->
                    let esc' = (not esc && ch == '\\')
                        nextState = if not esc && ch == '"' then NoStringState' else DoubleState esc'
                    in go (idx + 1) nextState False depth acc
                SingleState esc ->
                    let esc' = (not esc && ch == '\\')
                        nextState = if not esc && ch == '\'' then NoStringState' else SingleState esc'
                    in go (idx + 1) nextState False depth acc
                BacktickState' ->
                    let nextState = if ch == '`' then NoStringState' else BacktickState'
                    in go (idx + 1) nextState False depth acc

    collectCall openIdx = do
        name <- collectCallableName (openIdx - 1)
        (argsText, closeIdx) <- collectArgs (openIdx + 1) 1 NoStringState' False []
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

    collectArgs idx depth state escaped acc
        | idx >= len = Nothing
        | otherwise =
            let ch = input !! idx
            in case state of
                NoStringState' ->
                    case ch of
                        '"' -> collectArgs (idx + 1) depth (DoubleState False) False (ch:acc)
                        '\'' -> collectArgs (idx + 1) depth (SingleState False) False (ch:acc)
                        '`' -> collectArgs (idx + 1) depth BacktickState' False (ch:acc)
                        '(' -> collectArgs (idx + 1) (depth + 1) state False (ch:acc)
                        ')' ->
                            if depth == 1
                                then Just (reverse acc, idx)
                                else collectArgs (idx + 1) (depth - 1) state False (ch:acc)
                        _ -> collectArgs (idx + 1) depth state False (ch:acc)
                DoubleState esc ->
                    let esc' = (not esc && ch == '\\')
                        nextState = if not esc && ch == '"' then NoStringState' else DoubleState esc'
                    in collectArgs (idx + 1) depth nextState False (ch:acc)
                SingleState esc ->
                    let esc' = (not esc && ch == '\\')
                        nextState = if not esc && ch == '\'' then NoStringState' else SingleState esc'
                    in collectArgs (idx + 1) depth nextState False (ch:acc)
                BacktickState' ->
                    let nextState = if ch == '`' then NoStringState' else BacktickState'
                    in collectArgs (idx + 1) depth nextState False (ch:acc)

    data ReaderState
        = NoStringState'
        | DoubleState Bool
        | SingleState Bool
        | BacktickState'
        deriving (Eq)

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
            (revSuffix, _:revPrefix) -> reverse revPrefix

isStringLiteral :: String -> Bool
isStringLiteral s =
    (headMatch '"' && lastMatch '"') || (headMatch '`' && lastMatch '`')
  where
    headMatch c = not (null s) && head s == c
    lastMatch c = not (null s) && last s == c

isRuneLiteral :: String -> Bool
isRuneLiteral s = length s >= 2 && head s == '\'' && last s == '\''

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

showType :: Type -> String
showType (TypeName n) = n
showType UnknownType = "unknown"


