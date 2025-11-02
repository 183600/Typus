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
import Data.Char (isSpace, isDigit)
import Data.List (isInfixOf, isPrefixOf)

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
    concatMap analyzeDecl (zip [1..] gmDecls)
  where
    analyzeDecl (lineNum, decl) = case decl of
        GoVar (VarDecl ls _) -> concatMap (analyzeVarLine lineNum) ls
        GoStatement (StatementBlock ls) -> concatMap (analyzeStmtLine lineNum) ls
        GoFunc (FuncDecl ls) -> concatMap (analyzeStmtLine lineNum) ls
        _ -> []

analyzeVarLine :: Int -> String -> [ValueInfo]
analyzeVarLine lineNum line =
    let trimmed = trim line
    in if "var " `isPrefixOf` trimmed
        then case parseVarDecl trimmed of
            Just (names, initExpr) ->
                let kind = if isValueInit initExpr then ValueCopy else Unknown
                in [ValueInfo name kind lineNum | name <- names]
            Nothing -> []
        else []

analyzeStmtLine :: Int -> String -> [ValueInfo]
analyzeStmtLine lineNum line =
    let trimmed = trim line
    in if ":=" `isInfixOf` trimmed
        then case parseShortVarDecl trimmed of
            Just (names, initExpr) ->
                let kind = determineValueKind initExpr
                in [ValueInfo name kind lineNum | name <- names]
            Nothing -> []
        else []

parseVarDecl :: String -> Maybe ([String], String)
parseVarDecl line =
    let afterVarRaw = drop 4 line
        afterVar = dropWhile isSpace afterVarRaw
    in if null afterVar || head afterVar == '('
        then Nothing
        else
            let (lhs, rhsPart) = break (== '=') afterVar
                names = map (trim . dropTypeAnnotation) (splitByComma lhs)
                initExpr = case rhsPart of
                    ('=':rest) -> trim rest
                    _ -> ""
            in Just (filter (not . null) names, initExpr)
  where
    dropTypeAnnotation :: String -> String
    dropTypeAnnotation segment =
        case words segment of
            [] -> ""
            (n:_) -> trim n

parseShortVarDecl :: String -> Maybe ([String], String)
parseShortVarDecl line =
    case breakOn ":=" line of
        Nothing -> Nothing
        Just (lhs, rhs) ->
            let names = map trim (splitByComma lhs)
                initExpr = trim rhs
            in Just (filter (not . null) names, initExpr)

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack =
    case findIndex needle haystack of
        Nothing -> Nothing
        Just idx ->
            let (before, after) = splitAt idx haystack
                rest = drop (length needle) after
            in Just (before, rest)

findIndex :: String -> String -> Maybe Int
findIndex needle haystack = search 0 haystack
  where
    search _ [] = Nothing
    search i s
        | needle `isPrefixOf` s = Just i
        | otherwise = case s of
            [] -> Nothing
            (_:xs) -> search (i + 1) xs

splitByComma :: String -> [String]
splitByComma s = case break (== ',') s of
    (a, []) -> [a]
    (a, _ : b) -> a : splitByComma b

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
    (startsWith '"' s && endsWith '"' s)
    || (startsWith '`' s && endsWith '`' s)

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
        (w:_) -> isKnownValueType w && "{" `elem` t
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

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

startsWith :: Char -> String -> Bool
startsWith c (x:_) = c == x
startsWith _ _ = False

endsWith :: Char -> String -> Bool
endsWith c s = case reverse s of
    (x:_) -> c == x
    _ -> False
