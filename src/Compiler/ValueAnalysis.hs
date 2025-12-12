{-# LANGUAGE RecordWildCards #-}

module Compiler.ValueAnalysis (
    ValueInfo(..),
    ValueKind(..),
    analyzeValueSemantics,
    extractValueCopyVars,
    isValueType,
    isReferenceInit
) where

import Compiler.GoAst
import Compiler.GoParsing (nestingDelta, splitTopLevel, stripLineComment)
import qualified Compiler.GoVarSpec as GoVar
import Data.Char (isAlphaNum, isDigit, isSpace, isUpper, toLower)
import Data.List (dropWhileEnd, intercalate, isInfixOf, isPrefixOf, stripPrefix)
import qualified Data.List as List
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Text as T
import Utils (trim)

data ValueKind
    = ValueCopy
    | Reference
    | Unknown
    deriving (Eq, Show)

data ValueInfo = ValueInfo
    { viName :: String
    , viKind :: ValueKind
    , viLine :: Int
    } deriving (Eq, Show)

type ValueTypeSet = Set.Set String

analyzeValueSemantics :: GoModule -> [ValueInfo]
analyzeValueSemantics goModule@GoModule{..} =
    let valueTypes = collectValueTypeNames goModule
    in snd $ List.foldl' (step valueTypes) (1, []) gmDecls
  where
    step valueTypes (lineStart, acc) decl =
        let infos = analyzeDecl valueTypes decl lineStart
            nextLine = lineStart + length (flattenDeclLines decl)
        in (nextLine, acc ++ infos)

analyzeDecl :: ValueTypeSet -> GoDecl -> Int -> [ValueInfo]
analyzeDecl valueTypes decl lineStart = case decl of
    GoVar varDecl -> analyzeVarDecl valueTypes lineStart varDecl
    GoFunc (FuncDecl ls) -> analyzeFuncDecl valueTypes lineStart ls
    GoStatement (StatementBlock ls) -> analyzeStatementBlock valueTypes lineStart ls
    _ -> []

analyzeVarDecl :: ValueTypeSet -> Int -> VarDecl -> [ValueInfo]
analyzeVarDecl valueTypes lineStart varDecl =
    concatMap fromSpec (GoVar.parseVarDeclRawSpecs (Just lineStart) varDecl)
  where
    fromSpec GoVar.RawVarSpec{..} =
        let names = rvsNames
            values = rvsValues
            line = fromMaybe lineStart rvsLine
        in [ ValueInfo name (determineValueKind valueTypes (trim (selectExprForIndex names values idx))) line
           | (name, idx) <- zip names [0 ..]
           ]

analyzeFuncDecl :: ValueTypeSet -> Int -> [String] -> [ValueInfo]
analyzeFuncDecl _ _ [] = []
analyzeFuncDecl valueTypes lineStart (_header:body) =
    analyzeShortVars valueTypes (zip [lineStart + 1 ..] body)

analyzeStatementBlock :: ValueTypeSet -> Int -> [String] -> [ValueInfo]
analyzeStatementBlock valueTypes lineStart lines0 =
    analyzeShortVars valueTypes (zip [lineStart ..] lines0)

analyzeShortVars :: ValueTypeSet -> [(Int, String)] -> [ValueInfo]
analyzeShortVars valueTypes tuples =
    concatMap toInfos (collectShortVarInits tuples)
  where
    toInfos ShortVarInit{..} =
        [ ValueInfo name (determineValueKind valueTypes (trim (selectExprForIndex sviNames sviValues idx))) sviLine
        | (name, idx) <- zip sviNames [0 ..]
        ]

data ShortVarInit = ShortVarInit
    { sviLine :: Int
    , sviNames :: [String]
    , sviValues :: [String]
    } deriving (Eq, Show)

data ScanState
    = NoStringState
    | DoubleStringState Bool
    | SingleStringState Bool
    | BacktickState
    deriving (Eq)

collectShortVarInits :: [(Int, String)] -> [ShortVarInit]
collectShortVarInits = go []
  where
    go acc [] = reverse acc
    go acc ((lineNo, raw):rest) =
        let cleaned = stripLineComment raw
        in case findShortVarIndex cleaned of
            Nothing -> go acc rest
            Just idx ->
                let names = parseShortVarNames (take idx cleaned)
                in if null names
                    then go acc rest
                    else
                        let rhsInitial = drop (idx + 2) cleaned
                            (exprCombined, remaining) = collectExpression rhsInitial rest
                            segment = case splitTopLevel ';' exprCombined of
                                [] -> trim exprCombined
                                (first:_) -> first
                            normalizedSegment = trimBlockSuffix segment
                            rawValues = if null normalizedSegment then [] else splitTopLevel ',' normalizedSegment
                            values = map trim rawValues
                            initInfo = ShortVarInit
                                { sviLine = lineNo
                                , sviNames = names
                                , sviValues = values
                                }
                        in go (initInfo : acc) remaining

collectExpression :: String -> [(Int, String)] -> (String, [(Int, String)])
collectExpression initial rest = go [initial] (max 0 (nestingDelta initial)) rest
  where
    go :: [[Char]] -> Int -> [(a, String)] -> ([Char], [(a, String)])
    go parts depth remaining =
        let combined = intercalate "\n" parts
            trimmed = trim combined
        in if endsWithBlockStart trimmed
            then (combined, remaining)
            else if not (shouldContinue trimmed depth)
                then (combined, remaining)
                else case remaining of
                    [] -> (combined, [])
                    ((_, nextRaw) : nextRest) ->
                        let nextClean = stripLineComment nextRaw
                            depth' = max 0 (depth + nestingDelta nextClean)
                        in go (parts ++ [nextClean]) depth' nextRest

endsWithBlockStart :: String -> Bool
endsWithBlockStart txt =
    case reverse (dropWhileEnd isSpace txt) of
        '{' : ' ' : _ -> True
        _ -> False

shouldContinue :: String -> Int -> Bool
shouldContinue text depth
    | null text = True
    | depth > 0 = True
    | otherwise = case lastNonSpaceChar text of
        Nothing -> True
        Just c -> c `elem` continuationChars

continuationChars :: [Char]
continuationChars = ",+-*/.%([{&|^=<>!:"

lastNonSpaceChar :: String -> Maybe Char
lastNonSpaceChar = go . reverse
  where
    go [] = Nothing
    go (c:cs)
        | isSpace c = go cs
        | otherwise = Just c

parseShortVarNames :: String -> [String]
parseShortVarNames lhs =
    let stripped = stripContextPrefix lhs
    in filter (not . null) (map trim (splitTopLevel ',' stripped))

stripContextPrefix :: String -> String
stripContextPrefix text = dropLeadingParens (List.foldl' (flip dropKeyword) (trim text) keywords)
  where
    keywords = ["if", "for", "switch", "select"]

    dropKeyword keyword s
        | keyword `isPrefixOf` s && boundary (drop (length keyword) s) =
            dropWhile isSpace (drop (length keyword) s)
        | otherwise = s

    boundary [] = True
    boundary (c:_) = isSpace c || c == '('

    dropLeadingParens s =
        let trimmed = dropWhile isSpace s
        in case trimmed of
            '(' : rest -> dropWhile isSpace rest
            _ -> trimmed

trimBlockSuffix :: String -> String
trimBlockSuffix s =
    let trimmed = dropWhileEnd isSpace s
    in case reverse trimmed of
        '{' : ' ' : rest -> dropWhileEnd isSpace (reverse rest)
        _ -> trimmed

selectExprForIndex :: [String] -> [String] -> Int -> String
selectExprForIndex names values idx =
    case values of
        [] -> ""
        [single] -> single
        _
            | sameLength && idx < valueCount -> values !! idx
            | idx < valueCount -> values !! idx
            | otherwise -> intercalate ", " values
  where
    valueCount = length values
    sameLength = valueCount == length names

findShortVarIndex :: String -> Maybe Int
findShortVarIndex text = go 0 NoStringState
  where
    len = length text

    go idx state
        | idx >= len - 1 = Nothing
        | otherwise =
            let c = text !! idx
                next = text !! (idx + 1)
            in case state of
                NoStringState ->
                    case c of
                        '"' -> go (idx + 1) (DoubleStringState False)
                        '\'' -> go (idx + 1) (SingleStringState False)
                        '`' -> go (idx + 1) BacktickState
                        ':' | next == '=' -> Just idx
                        _ -> go (idx + 1) NoStringState
                DoubleStringState escaped ->
                    let escaped' = if escaped then False else c == '\\'
                        nextState = if not escaped && c == '"' then NoStringState else DoubleStringState escaped'
                    in go (idx + 1) nextState
                SingleStringState escaped ->
                    let escaped' = if escaped then False else c == '\\'
                        nextState = if not escaped && c == '\'' then NoStringState else SingleStringState escaped'
                    in go (idx + 1) nextState
                BacktickState ->
                    let nextState = if c == '`' then NoStringState else BacktickState
                    in go (idx + 1) nextState

determineValueKind :: ValueTypeSet -> String -> ValueKind
determineValueKind valueTypes expr
    | isValueInit valueTypes expr = ValueCopy
    | isReferenceInit expr = Reference
    | otherwise = Unknown

isValueInit :: ValueTypeSet -> String -> Bool
isValueInit valueTypes expr =
    let e = trim expr
    in isStringLiteral e
        || isBooleanLiteral e
        || isNumericLiteral e
        || isValueCompositeLiteral valueTypes e

isReferenceInit :: String -> Bool
isReferenceInit expr =
    let e = trim expr
    in "&" `isPrefixOf` e
        || "make(" `isPrefixOf` e
        || "new(" `isPrefixOf` e
        || isArrayLiteral e
        || isMapLiteral e

isStringLiteral :: String -> Bool
isStringLiteral s =
    let text = T.pack s
        doubleQuote = T.singleton '"'
        backtick = T.singleton '`'
    in (doubleQuote `T.isPrefixOf` text && doubleQuote `T.isSuffixOf` text)
        || (backtick `T.isPrefixOf` text && backtick `T.isSuffixOf` text)

isBooleanLiteral :: String -> Bool
isBooleanLiteral s = s `elem` ["true", "false"]

isNumericLiteral :: String -> Bool
isNumericLiteral s =
    case s of
        [] -> False
        (c:_) -> isDigit c || c == '-' || c == '+'

isValueCompositeLiteral :: ValueTypeSet -> String -> Bool
isValueCompositeLiteral valueTypes s =
    let t = trim s
        (prefix, suffix) = break (== '{') t
        typeCandidate = trim prefix
    in not (null suffix)
        && not (null typeCandidate)
        && isKnownValueType valueTypes typeCandidate

isKnownValueType :: ValueTypeSet -> String -> Bool
isKnownValueType valueTypes rawType =
    let normalized = normalizeTypeName rawType
    in not (null normalized) && Set.member normalized valueTypes

isArrayLiteral :: String -> Bool
isArrayLiteral s =
    let t = trim s
    in "[]" `isPrefixOf` t && not ("..." `isPrefixOf` drop 2 t)

isMapLiteral :: String -> Bool
isMapLiteral s = "map[" `isPrefixOf` trim s

extractValueCopyVars :: GoModule -> [String]
extractValueCopyVars goModule =
    [ viName info
    | info <- analyzeValueSemantics goModule
    , viKind info == ValueCopy
    ]

isValueType :: String -> Bool
isValueType = (`Set.member` builtInValueTypes) . trim

builtInValueTypes :: ValueTypeSet
builtInValueTypes = Set.fromList
    [ "int", "int8", "int16", "int32", "int64"
    , "uint", "uint8", "uint16", "uint32", "uint64"
    , "float32", "float64", "complex64", "complex128"
    , "bool", "byte", "rune", "string"
    ]

collectValueTypeNames :: GoModule -> ValueTypeSet
collectValueTypeNames GoModule{..} =
    List.foldl' step builtInValueTypes gmDecls
  where
    step acc decl = case decl of
        GoType typeDecl ->
            let custom = Set.fromList
                    [ tssName summary
                    | summary <- collectTypeSpecSummaries typeDecl
                    , isExportedValueType summary
                    ]
            in acc `Set.union` custom
        _ -> acc

data TypeSpecSummary = TypeSpecSummary
    { tssName :: String
    , tssRemainder :: String
    } deriving (Eq, Show)

collectTypeSpecSummaries :: TypeDecl -> [TypeSpecSummary]
collectTypeSpecSummaries TypeDecl{..}
    | typeIsGroup = parseGroupedSpecs typeLines
    | otherwise = maybeToList (parseSingleSpec typeLines)

parseSingleSpec :: [String] -> Maybe TypeSpecSummary
parseSingleSpec lines0 = do
    header <- firstMeaningfulTypeLine lines0
    parseTypeSpecSummary header

parseGroupedSpecs :: [String] -> [TypeSpecSummary]
parseGroupedSpecs lines0 =
    reverse (collectSpecs inner "" 0 [])
  where
    inner = drop 1 (dropWhileEnd isGroupClosing lines0)

    isGroupClosing line = trim (stripLineComment line) == ")"

    collectSpecs [] current _ acc
        | null (trim current) = acc
        | otherwise = maybeAdd current acc
    collectSpecs (raw:rest) current depth acc =
        let stripped = trim (stripLineComment raw)
            lowerStripped = map toLower stripped
        in if null stripped || "//" `isPrefixOf` stripped
            then collectSpecs rest current depth acc
            else if null current && not (shouldStartNewSpec stripped lowerStripped)
                then collectSpecs rest current depth acc
                else
                    let nextText = if null current then stripped else current ++ " " ++ stripped
                        depth' = depth + nestingDelta stripped
                        (acc', nextCurrent, nextDepth) =
                            if depth' <= 0
                                then (maybeAdd nextText acc, "", 0)
                                else (acc, nextText, depth')
                    in collectSpecs rest nextCurrent nextDepth acc'

    maybeAdd text acc =
        let candidate = trim text
        in case parseTypeSpecSummary candidate of
            Just summary -> summary : acc
            Nothing -> acc

    shouldStartNewSpec :: Foldable t => t Char -> [Char] -> Bool
    shouldStartNewSpec stripped lowerStripped =
        any isSpace stripped
            || '=' `elem` stripped
            || "struct" `isInfixOf` lowerStripped
            || "interface" `isInfixOf` lowerStripped

firstMeaningfulTypeLine :: [String] -> Maybe String
firstMeaningfulTypeLine [] = Nothing
firstMeaningfulTypeLine (line:rest) =
    let stripped = trim (stripLineComment line)
    in if null stripped || "//" `isPrefixOf` stripped
        then firstMeaningfulTypeLine rest
        else Just stripped

parseTypeSpecSummary :: String -> Maybe TypeSpecSummary
parseTypeSpecSummary raw =
    let cleaned = trim raw
        withoutKeyword =
            if "type " `isPrefixOf` cleaned
                then dropWhile isSpace (drop (length "type") cleaned)
                else cleaned
        (namePart, restAfterName) = span isTypeNameChar withoutKeyword
        remainder = dropWhile isSpace (dropTypeParameters restAfterName)
    in if null namePart
        then Nothing
        else Just TypeSpecSummary
            { tssName = namePart
            , tssRemainder = remainder
            }

isTypeNameChar :: Char -> Bool
isTypeNameChar c = isAlphaNum c || c == '_'

dropTypeParameters :: String -> String
dropTypeParameters text =
    let trimmed = dropWhile isSpace text
    in case trimmed of
        '[':rest -> dropWhile isSpace (dropBalanced 1 rest)
        _ -> trimmed
  where
    dropBalanced :: Int -> String -> String
    dropBalanced _ [] = []
    dropBalanced depth (x:xs)
        | x == '[' = dropBalanced (depth + 1) xs
        | x == ']' =
            if depth == 1
                then xs
                else dropBalanced (depth - 1) xs
        | otherwise = dropBalanced depth xs

isExportedValueType :: TypeSpecSummary -> Bool
isExportedValueType TypeSpecSummary{..} =
    case tssName of
        c:_ -> isUpper c && typeSummaryIndicatesValue tssRemainder
        [] -> False

typeSummaryIndicatesValue :: String -> Bool
typeSummaryIndicatesValue remainder =
    let cleaned = dropWhile isSpace remainder
        lowered = map toLower cleaned
        hasStruct = "struct" `isInfixOf` lowered
        hasInterface = "interface" `isInfixOf` lowered
        hasFunc = "func" `isPrefixOf` lowered
    in if hasStruct
        then True
        else if hasInterface || hasFunc
            then False
            else case stripPrefix "=" cleaned of
                Just restAlias -> aliasIndicatesValue restAlias
                Nothing -> not (isReferenceLike cleaned)

aliasIndicatesValue :: String -> Bool
aliasIndicatesValue aliasRaw =
    let target = dropWhile isSpace aliasRaw
        loweredTarget = map toLower target
    in if null target
        then True
        else if "struct" `isInfixOf` loweredTarget
            then True
            else not (isReferenceLike target)

isReferenceLike :: String -> Bool
isReferenceLike raw =
    case dropWhile isSpace raw of
        [] -> False
        ('*':_) -> True
        ('[':_) -> True
        other ->
            let loweredOther = map toLower other
            in "map[" `isPrefixOf` loweredOther
                || "chan" `isPrefixOf` loweredOther
                || "func(" `isPrefixOf` loweredOther
                || "func " `isPrefixOf` loweredOther
                || "interface" `isPrefixOf` loweredOther

normalizeTypeName :: String -> String
normalizeTypeName =
    dropPackageQualifier . stripGenericSuffix . stripPointerPrefix . trim

stripPointerPrefix :: String -> String
stripPointerPrefix = dropWhile (== '*')

stripGenericSuffix :: String -> String
stripGenericSuffix name =
    case break (== '[') name of
        (base, _) -> base

dropPackageQualifier :: String -> String
dropPackageQualifier name =
    let (revBase, revRest) = span (/= '.') (reverse name)
    in case revRest of
        [] -> reverse revBase
        (_:_) -> reverse revBase
