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
import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
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

analyzeValueSemantics :: GoModule -> [ValueInfo]
analyzeValueSemantics GoModule{..} =
    snd $ foldl' step (1, []) gmDecls
  where
    step (lineStart, acc) decl =
        let infos = analyzeDecl decl lineStart
            nextLine = lineStart + length (flattenDeclLines decl)
        in (nextLine, acc ++ infos)

analyzeDecl :: GoDecl -> Int -> [ValueInfo]
analyzeDecl decl lineStart = case decl of
    GoVar varDecl -> analyzeVarDecl lineStart varDecl
    GoFunc (FuncDecl ls) -> analyzeFuncDecl lineStart ls
    GoStatement (StatementBlock ls) -> analyzeStatementBlock lineStart ls
    _ -> []

analyzeVarDecl :: Int -> VarDecl -> [ValueInfo]
analyzeVarDecl lineStart varDecl =
    concatMap fromSpec (GoVar.parseVarDeclRawSpecs (Just lineStart) varDecl)
  where
    fromSpec GoVar.RawVarSpec{..} =
        let names = rvsNames
            values = rvsValues
            line = fromMaybe lineStart rvsLine
        in [ ValueInfo name (determineValueKind (trim (selectExprForIndex names values idx))) line
           | (name, idx) <- zip names [0 ..]
           ]

analyzeFuncDecl :: Int -> [String] -> [ValueInfo]
analyzeFuncDecl _ [] = []
analyzeFuncDecl lineStart (_header:body) =
    analyzeShortVars (zip [lineStart + 1 ..] body)

analyzeStatementBlock :: Int -> [String] -> [ValueInfo]
analyzeStatementBlock lineStart lines0 =
    analyzeShortVars (zip [lineStart ..] lines0)

analyzeShortVars :: [(Int, String)] -> [ValueInfo]
analyzeShortVars tuples =
    concatMap toInfos (collectShortVarInits tuples)
  where
    toInfos ShortVarInit{..} =
        [ ValueInfo name (determineValueKind (trim (selectExprForIndex sviNames sviValues idx))) sviLine
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
stripContextPrefix text = dropLeadingParens (foldl' (flip dropKeyword) (trim text) keywords)
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

determineValueKind :: String -> ValueKind
determineValueKind expr
    | isValueInit expr = ValueCopy
    | isReferenceInit expr = Reference
    | otherwise = Unknown

isValueInit :: String -> Bool
isValueInit expr =
    let e = trim expr
    in isStringLiteral e
        || isBooleanLiteral e
        || isNumericLiteral e
        || isValueCompositeLiteral e

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

isValueCompositeLiteral :: String -> Bool
isValueCompositeLiteral s =
    let t = trim s
    in case words t of
        (w:_) -> isKnownValueType w && '{' `elem` t
        _ -> False

isKnownValueType :: String -> Bool
isKnownValueType t =
    t `elem` ["int", "int8", "int16", "int32", "int64",
              "uint", "uint8", "uint16", "uint32", "uint64",
              "float32", "float64", "complex64", "complex128",
              "bool", "byte", "rune", "string"]

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
isValueType = isKnownValueType . trim
