module Compiler.DependentTypeChecker (
    checkDependentTypes,
    extractDependentTypeContent
) where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T

import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import DependentTypesParser (DependentTypeError(..), runDependentTypesParser, parserErrors)
import Compiler.Errors
    ( CompilerError
    , CompilerResult
    , CompilationPhase(..)
    , ErrorCategory(..)
    , ErrorSeverity(..)
    , mkCompilerError
    )
import SourceLocation
    ( Located(..)
    , SourceSpan
    , locatedValue
    , posAt
    , spanFrom
    )

directiveEnabled :: Maybe (Located Bool) -> Bool
directiveEnabled = maybe False locatedValue

checkDependentTypes :: TypusFile -> CompilerResult ()
checkDependentTypes typusFile =
    let directives = tfDirectives typusFile
        blocks = tfBlocks typusFile
        fileEnabled = directiveEnabled (fdDependentTypes directives)
        blockEnabled = any (directiveEnabled . bdDependentTypes . cbDirectives) blocks
        dependentContent = extractDependentTypeContent typusFile
        hasDependentBlocks = hasMeaningfulContent dependentContent
        shouldCheck = (fileEnabled || blockEnabled) && hasDependentBlocks
    in if not shouldCheck
           then Right ()
           else case runDependentTypesParser dependentContent of
               Left err -> Left [parserFailure err]
               Right (_, parser) ->
                   let errors = parserErrors parser
                   in if null errors
                         then Right ()
                         else Left (map toCompilerError errors)

extractDependentTypeContent :: TypusFile -> String
extractDependentTypeContent typusFile =
    let blocks = tfBlocks typusFile
        directives = tfDirectives typusFile
        fileEnabled = directiveEnabled (fdDependentTypes directives)
        explicitSegments =
            [ blockContent
            | block <- blocks
            , let blockDirs = cbDirectives block
            , Just locatedFlag <- [bdDependentTypes blockDirs]
            , locatedValue locatedFlag
            , let blockContent = cbContent block
            , hasMeaningfulContent blockContent
            ]
        implicitSegments
            | not fileEnabled = []
            | otherwise =
                [ segment
                | block <- blocks
                , let blockDirs = cbDirectives block
                , bdDependentTypes blockDirs == Nothing
                , segment <- extractImplicitDependentSegments (cbContent block)
                , hasMeaningfulContent segment
                , isLikelyDependentSegment segment
                ]
    in intercalate "\n" (explicitSegments ++ implicitSegments)

extractImplicitDependentSegments :: String -> [String]
extractImplicitDependentSegments content = go (lines content) []
  where
    go [] acc = reverse acc
    go (line:rest) acc =
        case classifySegmentStart line of
            Nothing -> go rest acc
            Just kind ->
                let (segmentText, remaining) = captureSegment kind line rest
                in go remaining (segmentText : acc)

data SegmentKind
    = AliasSegment
    | TypeSegment
    | FuncSegment
    deriving (Eq, Show)

classifySegmentStart :: String -> Maybe SegmentKind
classifySegmentStart line =
    let trimmed = trimLeading line
    in if "alias " `isPrefixOf` trimmed
          then Just AliasSegment
          else if isTypeCandidate trimmed
              then Just TypeSegment
              else if isFuncCandidate trimmed
                  then Just FuncSegment
                  else Nothing
  where
    isTypeCandidate txt =
        "type " `isPrefixOf` txt
        && (hasAngleGenerics txt || hasWhereClause txt)
    isFuncCandidate txt =
        "func " `isPrefixOf` txt
        && (containsArrow txt || hasWhereClause txt)
    hasAngleGenerics txt = '<' `elem` txt && '>' `elem` txt
    hasWhereClause txt = " where " `isInfixOf` txt
    containsArrow txt = "->" `isInfixOf` txt

captureSegment :: SegmentKind -> String -> [String] -> (String, [String])
captureSegment AliasSegment start rest =
    let (continuation, remaining) = span aliasContinuation rest
    in (unlines (start : continuation), remaining)
captureSegment TypeSegment start rest =
    let startDelta = braceDelta start
        needsBody = "struct" `isInfixOf` start || startDelta > 0
        (additional, remaining) = captureStructuredLines rest startDelta needsBody
    in (unlines (start : additional), remaining)
captureSegment FuncSegment start rest =
    let startDelta = braceDelta start
        (additional, remaining) = captureStructuredLines rest startDelta False
    in (unlines (start : additional), remaining)

aliasContinuation :: String -> Bool
aliasContinuation line =
    let trimmed = trimLeading line
    in not (null trimmed) && not (isBoundaryStart trimmed)

captureStructuredLines :: [String] -> Int -> Bool -> ([String], [String])
captureStructuredLines lines0 initialDepth needsBody =
    go lines0 initialDepth needsBody (initialDepth > 0) []
  where
    go [] _ _ _ acc = (reverse acc, [])
    go (line:rest) depth bodyRequired seenBody acc =
        let delta = braceDelta line
            depth' = depth + delta
            seenBody' = seenBody || delta /= 0 || depth > 0
            acc' = line : acc
            nextLine = listToMaybe rest
        in if shouldStop bodyRequired depth' seenBody' nextLine
              then (reverse acc', rest)
              else go rest depth' bodyRequired seenBody' acc'

shouldStop :: Bool -> Int -> Bool -> Maybe String -> Bool
shouldStop bodyRequired depth seenBody nextLine =
    case nextLine of
        Nothing -> not bodyRequired || depth <= 0 || not seenBody
        Just line ->
            let trimmed = trimLeading line
            in if bodyRequired
                  then seenBody && depth <= 0 && (null trimmed || isBoundaryStart trimmed)
                  else null trimmed || isBoundaryStart trimmed

braceDelta :: String -> Int
braceDelta = go 0 False False
  where
    go acc _ _ [] = acc
    go acc inString escaped (c:cs)
        | escaped = go acc inString False cs
        | c == '\\' && inString = go acc inString True cs
        | c == '"' = go acc (not inString) False cs
        | inString = go acc inString False cs
        | c == '{' = go (acc + 1) inString False cs
        | c == '}' = go (acc - 1) inString False cs
        | otherwise = go acc inString False cs

isBoundaryStart :: String -> Bool
isBoundaryStart txt =
    any (`isPrefixOf` txt)
        [ "package "
        , "import "
        , "func "
        , "type "
        , "var "
        , "const "
        ]

trimLeading :: String -> String
trimLeading = dropWhile isSpace

parserFailure :: String -> CompilerError
parserFailure errMsg =
    mkCompilerError
        "DT0000"
        (T.pack ("Failed to parse dependent type block: " ++ errMsg))
        DependentTypeCheckingPhase
        Constraint
        Error
        Nothing
        Nothing
        (map T.pack
            [ "Ensure dependent type blocks follow the documented syntax"
            , "Check for unmatched braces or directives"
            ])
        []
        Nothing

toCompilerError :: DependentTypeError -> CompilerError
toCompilerError err = case err of
    SyntaxError msg line snippet ->
        mkCompilerError
            "DT0001"
            (T.pack ("Syntax error: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            (Just (spanForLine line))
            (nonEmpty snippet)
            (map T.pack
                [ "Review the syntax near the reported line"
                , "Ensure nested constructs are properly closed"
                ])
            []
            Nothing
    InvalidTypeSyntax msg ->
        mkCompilerError
            "DT0002"
            (T.pack ("Invalid type syntax: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Verify type declarations inside dependent type blocks"])
            []
            Nothing
    MissingConstraint msg ->
        mkCompilerError
            "DT0003"
            (T.pack ("Missing constraint: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Add the required constraint to the dependent type declaration"])
            []
            Nothing
    InvalidParameter msg ->
        mkCompilerError
            "DT0004"
            (T.pack ("Invalid parameter: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Check the parameter list for the dependent type"])
            []
            Nothing
    ConstraintParseError msg ->
        mkCompilerError
            "DT0005"
            (T.pack ("Constraint parse error: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Ensure constraints use supported operators and syntax"])
            []
            Nothing
    TypeVariableError msg ->
        mkCompilerError
            "DT0006"
            (T.pack ("Type variable error: " ++ msg))
            DependentTypeCheckingPhase
            Constraint
            Error
            Nothing
            Nothing
            (map T.pack ["Ensure type variables are declared before use"])
            []
            Nothing

spanForLine :: Int -> SourceSpan
spanForLine lineNumber =
    let safeLine = max 1 lineNumber
    in spanFrom (posAt safeLine 1)

hasMeaningfulContent :: String -> Bool
hasMeaningfulContent = not . T.null . T.strip . T.pack

isLikelyDependentSegment :: String -> Bool
isLikelyDependentSegment segment =
    let trimmed = trimLeading segment
    in if not ("type " `isPrefixOf` trimmed)
          then True
          else
            case extractStructBody trimmed of
              Nothing -> True
              Just body ->
                  let bodyLines = lines body
                      hasDependentFields = any looksLikeDependentField bodyLines
                      hasGoFields = any looksLikeGoField bodyLines
                  in hasDependentFields || not hasGoFields

extractStructBody :: String -> Maybe String
extractStructBody input = do
    structStart <- findStructKeyword input
    let afterStruct = drop (length structKeyword) structStart
    afterBrace <- dropUntilChar '{' afterStruct
    collectStructBody 1 afterBrace []
  where
    structKeyword = "struct"

findStructKeyword :: String -> Maybe String
findStructKeyword [] = Nothing
findStructKeyword str@(_:rest)
    | structKeyword `isPrefixOf` str = Just str
    | otherwise = findStructKeyword rest
  where
    structKeyword = "struct"

dropUntilChar :: Char -> String -> Maybe String
dropUntilChar _ [] = Nothing
dropUntilChar target xs =
    case dropWhile (/= target) xs of
        [] -> Nothing
        (_:rest) -> Just rest

collectStructBody :: Int -> String -> String -> Maybe String
collectStructBody _ [] _ = Nothing
collectStructBody depth (c:cs) acc
    | c == '{' = collectStructBody (depth + 1) cs (c:acc)
    | c == '}' =
        let depth' = depth - 1
        in if depth' == 0
              then Just (reverse acc)
              else collectStructBody depth' cs ('}':acc)
    | otherwise = collectStructBody depth cs (c:acc)

looksLikeDependentField :: String -> Bool
looksLikeDependentField line =
    case trimLeading line of
        [] -> False
        trimmed | isCommentLine trimmed -> False
        trimmed ->
            let (ident, rest) = span isIdentifierChar trimmed
            in not (null ident)
                && isIdentifierStart (head ident)
                && case trimLeading rest of
                     ':' : after ->
                         case trimLeading after of
                             '=' : _ -> False
                             _ -> True
                     _ -> False

looksLikeGoField :: String -> Bool
looksLikeGoField line =
    case trimLeading line of
        [] -> False
        trimmed | isCommentLine trimmed -> False
        trimmed ->
            let (ident, rest) = span isIdentifierChar trimmed
                next = trimLeading rest
            in not (null ident)
                && isIdentifierStart (head ident)
                && not (null next)
                && head next /= ':'

isIdentifierStart :: Char -> Bool
isIdentifierStart c = isAlpha c || c == '_'

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

isCommentLine :: String -> Bool
isCommentLine txt = "//" `isPrefixOf` txt

nonEmpty :: String -> Maybe String
nonEmpty s = if null s then Nothing else Just s
