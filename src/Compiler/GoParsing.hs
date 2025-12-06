module Compiler.GoParsing (
    stripLineComment,
    splitTopLevel,
    nestingDelta,
    removeTrailingComma,
    findAssignmentIndex,
    consumeNames
) where

import Data.Char (isAlphaNum, isSpace)
import qualified Data.List as List
import Utils (trim)

data StrState = NoString | DoubleStr | SingleStr | BacktickStr
    deriving (Eq)

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

-- | Remove trailing // comments while respecting string literals.
stripLineComment :: String -> String
stripLineComment = go NoString False
  where
    go _ _ [] = []
    go state escaped (x:y:rest)
        | state == NoString && x == '/' && y == '/' = []
        | otherwise = x : goState state escaped y rest
    go _ _ [x] = [x]

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

-- | Split a string on the given delimiter, ignoring delimiters that appear
-- inside strings or balanced (), {}, [] groups.
splitTopLevel :: Char -> String -> [String]
splitTopLevel delim input = reverse (finalise current pieces)
  where
    (pieces, current, _) = List.foldl' step ([], [], NoStringState) input

    finalise cur acc =
        let piece = trim (reverse cur)
        in if null piece then acc else piece : acc

    step (acc, cur, state) ch =
        case updateState state ch of
            (newState, Just SplitHere)
                | nullInners newState -> (trim (reverse cur) : acc, [], newState)
            (newState, _) -> (acc, ch : cur, newState)

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

-- | Compute the aggregate change in nesting depth for parentheses,
-- brackets, and braces.
nestingDelta :: String -> Int
nestingDelta = List.foldl' step 0
  where
    step acc c = acc + delta c
    delta '(' = 1
    delta ')' = -1
    delta '[' = 1
    delta ']' = -1
    delta '{' = 1
    delta '}' = -1
    delta _ = 0

-- | Remove a trailing comma from the given string.
removeTrailingComma :: String -> String
removeTrailingComma = List.dropWhileEnd (== ',')

-- | Locate the index of the first standalone '=' assignment operator.
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

-- | Consume comma-separated identifier names from the start of the string.
consumeNames :: String -> ([String], String)
consumeNames s =
    case parseIdentifier s of
        Nothing -> ([], s)
        Just (ident, rest) ->
            let rest' = dropWhile isSpace rest
            in if null rest'
                then ([ident], rest')
                else gather rest' [ident]
  where
    gather text acc =
        let text' = dropWhile isSpace text
        in case text' of
            [] -> (acc, text')
            ',' : more ->
                case parseIdentifier (dropWhile isSpace more) of
                    Nothing -> (acc, more)
                    Just (nextName, afterNext) -> gather afterNext (acc ++ [nextName])
            _ -> (acc, text')

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
