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


-- Legacy tokenizer implementation moved to documentation for reference.
-- See docs/parser-history.md for details about the design evolution.

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
    go acc buildTagsRev [] = Right (acc, reverse buildTagsRev, [])
    go acc buildTagsRev (l:ls) =
        let t = trim l
        in if t == ""
            then go acc buildTagsRev ls
            else if isPrefixOf "//!" t
                then do
                    (k, v) <- parseFileDirectiveLine t
                    acc' <- updateFileDirective acc k v
                    go acc' buildTagsRev ls
            else if isBuildTagLine t
                then -- Preserve build tags by keeping them in the buildTags list
                     go acc (l:buildTagsRev) ls
                else Right (acc, reverse buildTagsRev, l:ls)
    
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
    -- accRev stores blocks in reverse order; codeBufRev stores lines in reverse order
    go :: [CodeBlock] -> [String] -> [String] -> Either String [CodeBlock]
    go accRev codeBufRev [] =
        let accRev' = flushCodeBufToAcc accRev codeBufRev
        in Right (reverse accRev')
    go accRev codeBufRev (l:ls) =
        let t = trim l
        in if startsWithBlockDirective t
           then do
             -- Flush any pending normal code into a default block
             let accRev' = flushCodeBufToAcc accRev codeBufRev
             -- Parse block directives
             kvs <- parseBlockDirectiveLine t
             bd <- parseBlockDirectives kvs
             -- Capture block content until the directive-closing '}'
             (blockLines, rest) <- captureDirectiveBlock ls
             let codeTxt = unlines blockLines
             let blk = CodeBlock bd codeTxt
             go (blk : accRev') [] rest
           else
             -- Accumulate normal code
             go accRev (l:codeBufRev) ls

    flushCodeBufToAcc :: [CodeBlock] -> [String] -> [CodeBlock]
    flushCodeBufToAcc accRev codeBufRev =
        case flushCodeBuf codeBufRev of
          Nothing -> accRev
          Just blk -> blk : accRev

    flushCodeBuf :: [String] -> Maybe CodeBlock
    flushCodeBuf bufRev =
        let codeTxt = trimRight (unlines (reverse bufRev))
        in if codeTxt == "" then Nothing else Just (CodeBlock defaultBlockDirectives codeTxt)

    startsWithBlockDirective :: String -> Bool
    startsWithBlockDirective s = isPrefixOf "{//!" (dropWhile isSpace s)

-- Capture lines of a directive block until we see the "extra }" that closes it.
-- We keep a brace-depth over code to avoid stopping at function's }.
-- The closing '}' for the directive block will make the depth go from 0 to -1.
captureDirectiveBlock :: [String] -> Either String ([String], [String])
captureDirectiveBlock = go 0 []
  where
    go :: Int -> [String] -> [String] -> Either String ([String], [String])
    go _ _ [] = Left "Unclosed directive block: missing closing '}'"
    go depth accRev (l:ls) =
        let newDepth = depth + curlyDelta l
        in if newDepth < 0
           then -- The last line l is the directive-closing '}', do not include it
                Right (reverse accRev, ls)
           else go newDepth (l:accRev) ls

-- Compute net curly-brace delta for a line, ignoring braces inside strings and line-comments.
-- Strings: "..." with support for escaping \" inside (approx).
-- Line comments: // ... (ignored).
curlyDelta :: String -> Int
curlyDelta = go False False 0
  where
    go :: Bool -> Bool -> Int -> String -> Int
    go _ _ acc [] = acc
    go inStr _ acc ('/':'/':_) | not inStr = acc  -- comment starts, ignore rest
    go inStr esc acc (c:cs)
        | inStr =
            case c of
                '"' | not esc -> go False False acc cs
                '\\'         -> go True True acc cs
                _              -> go True False acc cs
        | otherwise =
            case c of
                '"' -> go True False acc cs
                '{' -> go False False (acc + 1) cs
                '}' -> go False False (acc - 1) cs
                _   -> go False False acc cs

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
        -- Treat "constraints" as an alias for dependent_types at file level
        "constraints" -> Right fd { fdConstraints = Just boolValue
                                   , fdDependentTypes = Just boolValue }
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
        -- Treat "constraints" as an alias for dependent_types at block level too
        "constraints" -> Right bd { bdConstraints = boolValue
                                   , bdDependentTypes = boolValue }
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

-- Additional historical parser APIs are archived in docs/parser-history.md.
