{-# LANGUAGE RecordWildCards #-}

module Compiler.GoVarSpec (
    RawVarSpec(..),
    parseVarDeclRawSpecs,
    parseConstDeclRawSpecs,
    parseVarSpecRaw
) where

import Compiler.GoAst (ConstDecl(..), VarDecl(..))
import Compiler.GoParsing (consumeNames, findAssignmentIndex, nestingDelta, removeTrailingComma, splitTopLevel, stripLineComment)
import Control.Applicative ((<|>))
import Utils (trim)

-- | Lightweight representation of a variable specification extracted from Go syntax.
data RawVarSpec = RawVarSpec
    { rvsNames :: [String]
    , rvsType :: Maybe String
    , rvsValues :: [String]
    , rvsLine :: Maybe Int
    } deriving (Eq, Show)

-- | Parse variable declarations, optionally recording the starting line.
parseVarDeclRawSpecs :: Maybe Int -> VarDecl -> [RawVarSpec]
parseVarDeclRawSpecs start VarDecl{..} =
    parseDeclRawSpecs start varIsGroup varLines

-- | Parse constant declarations, optionally recording the starting line.
parseConstDeclRawSpecs :: Maybe Int -> ConstDecl -> [RawVarSpec]
parseConstDeclRawSpecs start ConstDecl{..} =
    parseDeclRawSpecs start constIsGroup constLines

-- | Parse a single variable specification from a textual line.
parseVarSpecRaw :: String -> Maybe RawVarSpec
parseVarSpecRaw rawLine =
    let line = trim (removeTrailingComma (stripLineComment rawLine))
        withoutKeyword
            | "var " `prefixOf` line = drop 4 line
            | "const " `prefixOf` line = drop 6 line
            | otherwise = line
    in if null withoutKeyword
        then Nothing
        else
            case findAssignmentIndex withoutKeyword of
                Nothing ->
                    let (names, remainder) = consumeNames withoutKeyword
                        typePart = trim remainder
                        cleanedNames = filter (not . null) names
                        mType = if null typePart then Nothing else Just typePart
                    in if null cleanedNames
                        then Nothing
                        else Just RawVarSpec
                                { rvsNames = cleanedNames
                                , rvsType = mType
                                , rvsValues = []
                                , rvsLine = Nothing
                                }
                Just idx ->
                    let (lhs, rhsRaw) = splitAt idx withoutKeyword
                        rhs = trim (drop 1 rhsRaw)
                        (names, remainder) = consumeNames lhs
                        typePart = trim remainder
                        values = map trim (splitTopLevel ',' rhs)
                        cleanedNames = filter (not . null) names
                        mType = if null typePart then Nothing else Just typePart
                    in if null cleanedNames
                        then Nothing
                        else Just RawVarSpec
                                { rvsNames = cleanedNames
                                , rvsType = mType
                                , rvsValues = values
                                , rvsLine = Nothing
                                }
  where
    prefixOf pref txt = pref == take (length pref) txt

-- Internal helpers -----------------------------------------------------------------

parseDeclRawSpecs :: Maybe Int -> Bool -> [String] -> [RawVarSpec]
parseDeclRawSpecs _ _ [] = []
parseDeclRawSpecs start True lines0 =
    let annotated = annotateLines start lines0
        inner = drop 1 (dropWhileEndBy closingLine annotated)
    in reverse (collect inner Nothing "" 0 [])
  where
    closingLine (_, line) = trim (stripLineComment line) == ")"

    collect [] mStart current _ acc
        | null (trim current) = acc
        | otherwise =
            case parseVarSpecRaw current of
                Nothing -> acc
                Just spec -> spec { rvsLine = mStart } : acc
    collect ((mLine, raw):rest) mStart current depth acc =
        let stripped = trim (stripLineComment raw)
        in if null stripped
            then collect rest mStart current depth acc
            else
                let nextCurrent = if null current then stripped else current ++ " " ++ stripped
                    depthDelta = nestingDelta stripped
                    newDepth = depth + depthDelta
                    startLine = mStart <|> mLine
                in if newDepth <= 0
                    then case parseVarSpecRaw nextCurrent of
                        Nothing -> collect rest Nothing "" 0 acc
                        Just spec ->
                            let specLine = startLine <|> mLine
                            in collect rest Nothing "" 0 (spec { rvsLine = specLine } : acc)
                    else collect rest startLine nextCurrent newDepth acc
parseDeclRawSpecs start False lines0 =
    case parseVarSpecRaw combined of
        Nothing -> []
        Just spec -> [spec { rvsLine = firstLine }]
  where
    annotated = annotateLines start lines0
    combined = unwords [ trim (stripLineComment line) | (_, line) <- annotated, not (null (trim line)) ]
    firstLine = case annotated of
        [] -> Nothing
        ((mLine, _):_) -> mLine

annotateLines :: Maybe Int -> [String] -> [(Maybe Int, String)]
annotateLines Nothing ls = [(Nothing, line) | line <- ls]
annotateLines (Just start) ls = zip (map Just [start .. start + length ls - 1]) ls

dropWhileEndBy :: ((Maybe Int, String) -> Bool) -> [(Maybe Int, String)] -> [(Maybe Int, String)]
dropWhileEndBy p = reverse . dropWhile p . reverse
