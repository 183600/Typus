{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives) where

import Data.Char (isSpace)

-- ============================================================================
-- Data Types
-- ============================================================================

data FileDirectives = FileDirectives
    { fdOwnership :: Maybe Bool
    , fdDependentTypes :: Maybe Bool
    , fdConstraints :: Maybe Bool
    } deriving (Show, Eq)

data BlockDirectives = BlockDirectives
    { bdOwnership :: Bool
    , bdDependentTypes :: Bool
    , bdConstraints :: Bool
} deriving (Show, Eq)

data CodeBlock = CodeBlock
    { cbDirectives :: BlockDirectives
    , cbContent :: String
    } deriving (Show, Eq)

data TypusFile = TypusFile
    { tfDirectives :: FileDirectives
    , tfBuildTags :: [String]  -- Go build tags like //go:build and // +build
    , tfBlocks :: [CodeBlock]
    } deriving (Show, Eq)

-- Default values
defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives Nothing Nothing Nothing

defaultBlockDirectives :: BlockDirectives
defaultBlockDirectives = BlockDirectives False False False


-- ============================================================================
-- Tokenizer (kept and fixed; parser below uses a robust line-based approach)
-- ============================================================================

data Token
    = TIdentifier String
    | TStringLiteral String
    | TNumberLiteral String
    | TSymbol String
    | TDirective String  -- For parsing directives like //! ownership: on
    | TComment String
    | TWhitespace
    | TEOF
    deriving (Show, Eq)

-- Tokenizer state
data TokenizerState = TokenizerState
    { tsInput :: String
    , tsPosition :: Int
    , tsLine :: Int
    , tsColumn :: Int
    } deriving (Show)


{-
-- Create initial tokenizer state
initTokenizer :: String -> TokenizerState
initTokenizer input = TokenizerState
    { tsInput = input
    , tsPosition = 0
    , tsLine = 1
    , tsColumn = 1
    }
-}

{-
-- Get current character
currentChar :: Tokenizer (Maybe Char)
currentChar = do
    input <- gets tsInput
    case input of
        [] -> return Nothing
        (c:_) -> return (Just c)
-}

{-
-- Advance to next character
advance :: Tokenizer ()
advance = do
    st <- get
    case tsInput st of
        [] -> return ()
        (c:rest) -> put st
            { tsInput = rest
            , tsPosition = tsPosition st + 1
            , tsColumn = if c == '\n' then 1 else tsColumn st + 1
            , tsLine = if c == '\n' then tsLine st + 1 else tsLine st
            }
-}

{-
-- Tokenizer error
tokenizerError :: String -> Tokenizer a
tokenizerError msg = do
    pos <- gets tsPosition
    lift $ Left $ "Tokenizer error at position " ++ show pos ++ ": " ++ msg
-}

{-
-- Skip whitespace
skipWhitespace :: Tokenizer ()
skipWhitespace = do
    mc <- currentChar
    case mc of
        Just c | isSpace c -> advance >> skipWhitespace
        _ -> return ()
-}

{-
-- Parse identifier
parseIdentifier :: Tokenizer Token
parseIdentifier = do
    mc <- currentChar
    case mc of
        Just c | isAlpha c || c == '_' -> do
            ident <- parseIdentifier'
            return $ TIdentifier ident
        _ -> tokenizerError "Expected identifier"
-}

{-/*
-- Helper for parsing identifier
parseIdentifier' :: Tokenizer String
parseIdentifier' = do
    mc <- currentChar
    case mc of
        Just c | isAlphaNum c || c == '_' -> do
            advance
            rest <- parseIdentifier'
            return (c:rest)
        _ -> return "" */
-}

{-
-- Check if character is alpha
isAlpha :: Char -> Bool
isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
-}

{-
-- Check if character is alphanumeric
isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || (c >= '0' && c <= '9')
-}

{-
-- Parse string literal
parseStringLiteral :: Tokenizer Token
parseStringLiteral = do
    mc <- currentChar
    case mc of
        Just '"' -> do
            advance  -- Skip opening quote
            str <- parseStringContent
            return $ TStringLiteral str
        _ -> tokenizerError "Expected string literal"
-}

{-
-- Parse string content
parseStringContent :: Tokenizer String
parseStringContent = do
    mc <- currentChar
    case mc of
        Just '"' -> do
            advance  -- Skip closing quote
            return ""
        Just '\\' -> do
            advance  -- Skip escape character
            mc' <- currentChar
            case mc' of
                Just c -> do
                    advance
                    rest <- parseStringContent
                    return (c:rest)
                Nothing -> tokenizerError "Unexpected end of input in string literal"
        Just c -> do
            advance
            rest <- parseStringContent
            return (c:rest)
        Nothing -> tokenizerError "Unexpected end of input in string literal"
-}

{-
-- Parse number literal
parseNumberLiteral :: Tokenizer Token
parseNumberLiteral = do
    mc <- currentChar
    case mc of
        Just c | isDigit c -> do
            num <- parseNumberContent
            return $ TNumberLiteral num
        _ -> tokenizerError "Expected number literal"
-}

{-
-- Helper for parsing number content
parseNumberContent :: Tokenizer String
parseNumberContent = do
    mc <- currentChar
    case mc of
        Just c | isDigit c -> do
            advance
            rest <- parseNumberContent
            return (c:rest)
        _ -> return ""
-}

{-
-- Check if character is digit
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
-}

{-
-- Parse symbol
parseSymbol :: Tokenizer Token
parseSymbol = do
    mc <- currentChar
    case mc of
        Just c | isSymbolChar c -> do
            sym <- parseSymbol'
            return $ TSymbol sym
        _ -> tokenizerError "Expected symbol"

{-
-- Check if character is symbol character
isSymbolChar :: Char -> Bool
isSymbolChar c = c `elem` "{}()[];,.*+-/=<>!&|:"
-}

{-
-- Helper for parsing symbol
parseSymbol' :: Tokenizer String
parseSymbol' = do
    mc <- currentChar
    case mc of
        Just c | isSymbolChar c -> do
            advance
            rest <- parseSymbol'
            return (c:rest)
        _ -> return ""
-}

{-
-- Parse directive (starts with //!)
parseDirective :: Tokenizer Token
parseDirective = do
    input <- gets tsInput
    if "//!" `isPrefixOf` input
        then do
            replicateM_ 3 advance
            content <- parseLineContent
            return $ TDirective content
        else tokenizerError "Expected directive marker //!"
-}

{-
-- Parse comment (starts with // but not //!)
parseComment :: Tokenizer Token
parseComment = do
    input <- gets tsInput
    if "//" `isPrefixOf` input && not ("//!" `isPrefixOf` input)
        then do
            replicateM_ 2 advance
            content <- parseLineContent
            return $ TComment content
        else tokenizerError "Expected comment marker //"
-}

{-
-- Helper to parse content until end of line
parseLineContent :: Tokenizer String
parseLineContent = do
    mc <- currentChar
    case mc of
        Just '\n' -> return ""  -- End of line
        Just c -> do
            advance
            rest <- parseLineContent
            return (c:rest)
        Nothing -> return ""  -- End of input
-}

{-
-- Main tokenization function (fixed order for / vs symbol; doesn't emit TWhitespace)
tokenize :: String -> Either String [Token]
tokenize input = evalStateT (tokenize') (initTokenizer input)
  where
    tokenize' :: Tokenizer [Token]
    tokenize' = do
        skipWhitespace
        mc <- currentChar
        case mc of
            Nothing -> return [TEOF]
            Just '/' -> do
                input' <- gets tsInput
                tok <- if "//!" `isPrefixOf` input'
                      then parseDirective
                      else if "//" `isPrefixOf` input'
                           then parseComment
                           else parseSymbol
                rest <- tokenize'
                return (tok:rest)
            Just '"' -> do
                tok <- parseStringLiteral
                rest <- tokenize'
                return (tok:rest)
            Just c
                | isDigit c -> do
                    tok <- parseNumberLiteral
                    rest <- tokenize'
                    return (tok:rest)
                | isAlpha c || c == '_' -> do
                    tok <- parseIdentifier
                    rest <- tokenize'
                    return (tok:rest)
                | isSymbolChar c -> do
                    tok <- parseSymbol
                    rest <- tokenize'
                    return (tok:rest)
                | isSpace c -> do
                    -- skipWhitespace already consumed them; just continue
                    advance
                    tokenize'
                | otherwise -> tokenizerError ("Unexpected character: " ++ [c])
-}
-}

-- ============================================================================
-- Parser (robust, line-based parsing for directives and blocks)
-- ============================================================================

-- Public API

-- High-level parser that works line-by-line to handle directives and blocks.
parseTypus :: String -> Either String TypusFile
parseTypus input = do
    let ls = map stripCR (lines input)
    (fileDirs, buildTags, rest) <- parseFileDirectivesFromLines ls
    blocks <- parseBlocksFromLines rest
    return $ TypusFile fileDirs buildTags blocks

-- Parse file-level directives at the very top of file.
-- It skips leading blank lines, and accepts consecutive "//! key: value" lines.
-- Also preserves Go build tags (//go:build and // +build) at the very top.
-- Stops at the first non-blank non-file-directive non-build-tag line.
parseFileDirectivesFromLines :: [String] -> Either String (FileDirectives, [String], [String])
parseFileDirectivesFromLines = go defaultFileDirectives []
  where
    go :: FileDirectives -> [String] -> [String] -> Either String (FileDirectives, [String], [String])
    go acc buildTags [] = Right (acc, buildTags, [])
    go acc buildTags (l:ls) =
        let t = trim l
        in if t == ""
            then go acc buildTags ls
            else if isPrefixOf "//!" t
                then do
                    (k, v) <- parseFileDirectiveLine t
                    acc' <- updateFileDirective acc k v
                    go acc' buildTags ls
            else if isBuildTagLine t
                then -- Preserve build tags by keeping them in the buildTags list
                     go acc (buildTags ++ [l]) ls
                else Right (acc, buildTags, l:ls)
    
    isBuildTagLine line = 
        isPrefixOf "//go:build" line || isPrefixOf "// +build" line

-- Parse blocks:
-- - Normal code is accumulated as a default block (if non-empty).
-- - "{//! ...}" starts a directive block; we gather lines until the "extra }"
--   that closes the directive block. Any inner code braces are balanced and
--   do not end the directive block prematurely.
parseBlocksFromLines :: [String] -> Either String [CodeBlock]
parseBlocksFromLines = go [] []
  where
    -- go accBlocks accCodeBuf remainingLines
    go :: [CodeBlock] -> [String] -> [String] -> Either String [CodeBlock]
    go acc blocksBuf [] =
        let finalBlocks = acc ++ flushCodeBuf blocksBuf
        in Right finalBlocks
    go acc blocksBuf (l:ls) =
        let t = trim l
        in if startsWithBlockDirective t
            then do
                -- Flush any pending normal code into a default block
                let acc' = acc ++ flushCodeBuf blocksBuf
                -- Parse block directives
                kvs <- parseBlockDirectiveLine t
                bd <- parseBlockDirectives kvs
                -- Capture block content until the directive-closing '}'
                (blockLines, rest) <- captureDirectiveBlock ls
                let codeTxt = unlines blockLines
                let blk = CodeBlock bd codeTxt
                go (acc' ++ [blk]) [] rest
            else
                -- Accumulate normal code
                go acc (blocksBuf ++ [l]) ls

    flushCodeBuf :: [String] -> [CodeBlock]
    flushCodeBuf buf =
        let codeTxt = trimRight (unlines buf)
        in if codeTxt == "" then [] else [CodeBlock defaultBlockDirectives codeTxt]

    startsWithBlockDirective :: String -> Bool
    startsWithBlockDirective s = isPrefixOf "{//!" (dropWhile isSpace s)

-- Capture lines of a directive block until we see the "extra }" that closes it.
-- We keep a brace-depth over code to avoid stopping at function's }.
-- The closing '}' for the directive block will make the depth go from 0 to -1.
captureDirectiveBlock :: [String] -> Either String ([String], [String])
captureDirectiveBlock = go 0 []
  where
    go :: Int -> [String] -> [String] -> Either String ([String], [String])
    go _ _acc [] = Left "Unclosed directive block: missing closing '}'"
    go depth acc (l:ls) =
        let d = curlyDelta l
            newDepth = depth + d
        in if newDepth < 0
            then -- The last line l is the directive-closing '}', do not include it
                 Right (acc, ls)
            else go newDepth (acc ++ [l]) ls

-- Compute net curly-brace delta for a line, ignoring braces inside strings and line-comments.
-- Strings: "..." with support for escaping \" inside (approx).
-- Line comments: // ... (ignored).
curlyDelta :: String -> Int
curlyDelta = go False False 0 0
  where
    go :: Bool -> Bool -> Int -> Int -> String -> Int
    go _ _ acc _ [] = acc
    go inStr _esc acc _ ('/':'/':_) | not inStr = acc  -- comment starts, ignore rest
    go inStr _esc acc _prev (c:cs)
        | inStr =
            case c of
                '"' | not _esc -> go False False acc (fromEnum c) cs
                '\\' -> go True True acc (fromEnum c) cs
                _    -> go True False acc (fromEnum c) cs
        | otherwise =
            case c of
                '"'  -> go True False acc (fromEnum c) cs
                '{'  -> go False False (acc + 1) (fromEnum c) cs
                '}'  -> go False False (acc - 1) (fromEnum c) cs
                _    -> go False False acc (fromEnum c) cs

-- ============================================================================
-- Directive Parsing
-- ============================================================================

parseFileDirectiveLine :: String -> Either String (String, String)
parseFileDirectiveLine line =
    let directivePart = trim (drop 3 (dropWhile isSpace line))  -- Drop "//!"
    in case break (== ':') directivePart of
        (key, ':':value) -> Right (trim key, trim value)
        _ -> Left $ "Invalid file directive format: " ++ line

parseBlockDirectiveLine :: String -> Either String [(String, String)]
parseBlockDirectiveLine line = do
    -- Expect a line like: {//! ownership: off, constraints: on}
    let tline = dropWhile isSpace line
    if not ("{//!" `isPrefixOf` tline)
       then Left $ "Invalid block directive line (missing {//!): " ++ line
       else do
           -- Extract content between {//! and the matching }
           let afterPrefix = trim (drop 4 tline)  -- drop "{//!"
           let content = takeWhile (/= '}') afterPrefix
           if null (trim content)
           then Right []
           else mapM parseKeyValue $ splitOn ',' content
  where
    parseKeyValue :: String -> Either String (String, String)
    parseKeyValue s = case break (== ':') (trim s) of
        (key, ':':value) -> Right (trim key, trim value)
        _ -> Left $ "Invalid key:value format: " ++ s

updateFileDirective :: FileDirectives -> String -> String -> Either String FileDirectives
updateFileDirective fd key value = do
    boolValue <- parseBool value
    case key of
        "ownership" -> Right fd { fdOwnership = Just boolValue }
        "dependent_types" -> Right fd { fdDependentTypes = Just boolValue }
        "constraints" -> Right fd { fdConstraints = Just boolValue }
        _ -> Left $ "Unknown file directive: " ++ key

parseBlockDirectives :: [(String, String)] -> Either String BlockDirectives
parseBlockDirectives [] = Right defaultBlockDirectives
parseBlockDirectives ((key, value):kvs) = do
    bd <- updateBlockDirective defaultBlockDirectives (key, value)
    parseBlockDirectives' bd kvs

parseBlockDirectives' :: BlockDirectives -> [(String, String)] -> Either String BlockDirectives
parseBlockDirectives' bd [] = Right bd
parseBlockDirectives' bd ((key, value):kvs) = do
    bd' <- updateBlockDirective bd (key, value)
    parseBlockDirectives' bd' kvs

updateBlockDirective :: BlockDirectives -> (String, String) -> Either String BlockDirectives
updateBlockDirective bd (key, value) = do
    boolValue <- parseBool value
    case key of
        "ownership" -> Right bd { bdOwnership = boolValue }
        "dependent_types" -> Right bd { bdDependentTypes = boolValue }
        "constraints" -> Right bd { bdConstraints = boolValue }
        _ -> Left $ "Unknown block directive: " ++ key

parseBool :: String -> Either String Bool
parseBool s = case trim s of
    "on" -> Right True
    "off" -> Right False
    "true" -> Right True
    "false" -> Right False
    v -> Left $ "Invalid boolean value: " ++ v

-- ============================================================================
-- Utility Functions
-- ============================================================================

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . dropWhile (`elem` "\r\n") . reverse

{-
breakOn :: String -> String -> (String, String)
breakOn delimiter str = go str
  where
    go [] = ("", "")
    go s@(x:xs)
        | delimiter `isPrefixOf` s = ("", s)
        | otherwise = let (before, after) = go xs in (x:before, after)
-}

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = case break (== delim) str of
    (before, []) -> [before]
    (before, _:after) -> before : splitOn delim after

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

{-
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'
-}

stripCR :: String -> String
stripCR = reverse . dropWhile (== '\r') . reverse

-- ============================================================================
-- Legacy parser state/types (kept for completeness, not used by line-based parser)
-- ============================================================================

data ParserState = ParserState
    { psTokens :: [Token]
    , psPosition :: Int
    } deriving (Show)


{-
initParser :: [Token] -> ParserState
initParser tokens = ParserState
    { psTokens = tokens
    , psPosition = 0
    }
-}

{-
currentToken :: Parser Token
currentToken = do
    tokens <- gets psTokens
    case tokens of
        [] -> return TEOF
        (t:_) -> return t
-}

{-
advanceToken :: Parser ()
advanceToken = do
    st <- get
    case psTokens st of
        [] -> return ()
        (_:rest) -> put st { psTokens = rest }
-}

{-
parserError :: String -> Parser a
parserError msg = do
    pos <- gets psPosition
    lift $ Left $ "Parser error at token " ++ show pos ++ ": " ++ msg
-}

{-
expectToken :: Token -> Parser ()
expectToken expected = do
    actual <- currentToken
    if actual == expected
        then advanceToken
        else parserError $ "Expected " ++ show expected ++ ", but got " ++ show actual
-}

{-
expectIdentifier :: Parser String
expectIdentifier = do
    token <- currentToken
    case token of
        TIdentifier name -> do
            advanceToken
            return name
        _ -> parserError $ "Expected identifier, but got " ++ show token
-}

-- ============================================================================
-- Test the parser
-- ============================================================================

{-
testParser :: IO ()
testParser = do
    let testCases =
            [ ( "Simple function"
              , unlines
                    [ "package main"
                    , ""
                    , "func main() {"
                    , "    println(\"Hello\")"
                    , "}"
                    ]
              )
            , ( "With file directives"
              , unlines
                    [ "//! ownership: on"
                    , "//! dependent_types: off"
                    , ""
                    , "func test() {"
                    , "    return 42"
                    , "}"
                    ]
              )
            , ( "With block directives"
              , unlines
                    [ "{//! ownership: off}"
                    , "func special() {"
                    , "    // Special function"
                    , "}"
                    , "}"
                    ]
              )
            , ( "Nested braces"
              , unlines
                    [ "func nested() {"
                    , "    if true {"
                    , "        while false {"
                    , "            break"
                    , "        }"
                    , "    }"
                    , "}"
                    ]
              )
            , ( "Mixed directives"
              , unlines
                    [ "//! ownership: on"
                    , "//! dependent_types: off"
                    , ""
                    , "// Normal code"
                    , "func normal() {"
                    , "    x := 10"
                    , "}"
                    , ""
                    , "{//! constraints: on, ownership: off}"
                    , "func constrained() {"
                    , "    // With constraints"
                    , "    assert(x > 0)"
                    , "}"
                    , "}"
                    ]
              )
            ]

    mapM_ (\(name, code) -> do
        putStrLn $ "\n=== Testing: " ++ name ++ " ==="
        case parse code of
            Left err -> putStrLn $ "ERROR: " ++ err
            Right result -> do
                putStrLn "SUCCESS"
        ) testCases
-}

{-
-- Run the test
main :: IO ()
main = testParser
-}