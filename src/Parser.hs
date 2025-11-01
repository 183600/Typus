{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Parser
  ( parseTypus
  , FileDirectives(..)
  , BlockDirectives(..)
  , CodeBlock(..)
  , TypusFile(..)
  , defaultFileDirectives
  , defaultBlockDirectives
  ) where

import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import SourceLocation
  ( Located(..)
  , SourcePos(..)
  , SourceSpan(..)
  , locatedWithSpan
  , spanEnd
  , spanStart
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec (Parsec, errorBundlePretty)

-- ============================================================================
-- Data Types
-- ============================================================================

data FileDirectives = FileDirectives
    { fdOwnership :: Maybe (Located Bool)
    , fdDependentTypes :: Maybe (Located Bool)
    , fdConstraints :: Maybe (Located Bool)
    } deriving (Show, Eq)

data BlockDirectives = BlockDirectives
    { bdOwnership :: Maybe (Located Bool)
    , bdDependentTypes :: Maybe (Located Bool)
    , bdConstraints :: Maybe (Located Bool)
    } deriving (Show, Eq)

data CodeBlock = CodeBlock
    { cbDirectives :: BlockDirectives
    , cbContent :: String
    , cbSpan :: SourceSpan
    } deriving (Show, Eq)

data TypusFile = TypusFile
    { tfDirectives :: FileDirectives
    , tfBuildTags :: [Located String]
    , tfBlocks :: [CodeBlock]
    } deriving (Show, Eq)

-- Default values
defaultFileDirectives :: FileDirectives
defaultFileDirectives = FileDirectives Nothing Nothing Nothing

defaultBlockDirectives :: BlockDirectives
defaultBlockDirectives = BlockDirectives Nothing Nothing Nothing

-- ============================================================================
-- Parser entry point
-- ============================================================================

type MegaParser = Parsec Void String

parseTypus :: String -> Either String TypusFile
parseTypus input = do
    parsedLines <- case MP.runParser parseDocument "<input>" input of
      Left bundle -> Left (errorBundlePretty bundle)
      Right ls    -> Right ls
    buildTypusFile parsedLines

-- ============================================================================
-- Megaparsec-backed line capture
-- ============================================================================

data ParsedLine = ParsedLine
    { plText :: String
    , plEnding :: String
    , plSpan :: SourceSpan
    }

parseDocument :: MegaParser [ParsedLine]
parseDocument = MP.manyTill parseLine MP.eof

parseLine :: MegaParser ParsedLine
parseLine = do
    MP.notFollowedBy MP.eof
    startPos <- MP.getSourcePos
    startOffset <- MP.getOffset
    content <- MP.takeWhileP (Just "line content") (`notElem` ['\n', '\r'])
    lineEnding <- MP.optional . MP.try $ MP.string "\r\n"
    ending <- case lineEnding of
      Just crlf -> pure crlf
      Nothing   -> fromMaybe "" <$> MP.optional (MP.string "\n" MP.<|> MP.string "\r")
    endPos <- MP.getSourcePos
    endOffset <- MP.getOffset
    let span = SourceSpan (toSourcePos startPos startOffset) (toSourcePos endPos endOffset)
    pure ParsedLine
      { plText = content
      , plEnding = ending
      , plSpan = span
      }

-- Convert Megaparsec SourcePos to project SourcePos
toSourcePos :: MP.SourcePos -> Int -> SourcePos
toSourcePos pos offset = SourcePos
    { posLine = MP.unPos (MP.sourcePosLine pos)
    , posColumn = MP.unPos (MP.sourcePosColumn pos)
    , posOffset = offset
    }

-- ============================================================================
-- High-level assembly
-- ============================================================================

buildTypusFile :: [ParsedLine] -> Either String TypusFile
buildTypusFile lines0 = do
    (fileDirs, buildTags, rest) <- parseFileDirectivesFromParsedLines lines0
    blocks <- parseBlocksFromParsedLines rest
    pure TypusFile
      { tfDirectives = fileDirs
      , tfBuildTags = buildTags
      , tfBlocks = blocks
      }

-- ============================================================================
-- File directive parsing (top-of-file)
-- ============================================================================

parseFileDirectivesFromParsedLines
  :: [ParsedLine]
  -> Either String (FileDirectives, [Located String], [ParsedLine])
parseFileDirectivesFromParsedLines = go defaultFileDirectives []
  where
    go acc buildTagsRev [] = Right (acc, reverse buildTagsRev, [])
    go acc buildTagsRev (line:rest) =
      let trimmed = trim (plText line)
      in if trimmed == ""
           then go acc buildTagsRev rest
           else if isPrefixOf "//!" trimmed
             then do
               (key, val) <- parseFileDirectiveLine line
               acc' <- updateFileDirective acc key val
               go acc' buildTagsRev rest
           else if isBuildTagLine trimmed
             then let tag = locatedWithSpan (plSpan line) (plText line)
                  in go acc (tag : buildTagsRev) rest
             else Right (acc, reverse buildTagsRev, line:rest)

    isBuildTagLine t =
      isPrefixOf "//go:build" t || isPrefixOf "// +build" t

parseFileDirectiveLine :: ParsedLine -> Either String (String, Located Bool)
parseFileDirectiveLine ParsedLine{..} = do
    let stripped = dropWhile isSpace plText
    if not (isPrefixOf "//!" stripped)
      then Left $ "Invalid file directive format: " ++ plText
      else do
        let directivePart = trim (drop 3 stripped)
        case break (== ':') directivePart of
          (keyRaw, ':' : valueRaw) -> do
            boolValue <- parseBool valueRaw
            let key = trim keyRaw
            pure (key, locatedWithSpan plSpan boolValue)
          _ -> Left $ "Invalid file directive format: " ++ plText

updateFileDirective :: FileDirectives -> String -> Located Bool -> Either String FileDirectives
updateFileDirective fd key value = case key of
    "ownership" -> Right fd { fdOwnership = Just value }
    "dependent_types" -> Right fd { fdDependentTypes = Just value }
    "constraints" -> Right fd { fdConstraints = Just value
                                , fdDependentTypes = Just value }
    _ -> Left $ "Unknown file directive: " ++ key

-- ============================================================================
-- Block parsing
-- ============================================================================

parseBlocksFromParsedLines :: [ParsedLine] -> Either String [CodeBlock]
parseBlocksFromParsedLines = go [] []
  where
    go accRev codeBufRev [] =
      let accRev' = flushCodeBufToAcc accRev codeBufRev
      in Right (reverse accRev')
    go accRev codeBufRev (line:rest) =
      let trimmed = trim (plText line)
      in if startsWithBlockDirective trimmed
           then do
             let accWithCode = flushCodeBufToAcc accRev codeBufRev
             directivesPairs <- parseBlockDirectiveLine line
             directives <- parseBlockDirectives directivesPairs
             (blockLines, blockSpan, remaining) <- captureDirectiveBlock (plSpan line) rest
             let content = buildBlockContent blockLines
             let block = CodeBlock
                   { cbDirectives = directives
                   , cbContent = content
                   , cbSpan = blockSpan
                   }
             go (block : accWithCode) [] remaining
           else go accRev (line : codeBufRev) rest

    flushCodeBufToAcc accRev codeBufRev =
      case flushCodeBuf codeBufRev of
        Nothing -> accRev
        Just blk -> blk : accRev

flushCodeBuf :: [ParsedLine] -> Maybe CodeBlock
flushCodeBuf [] = Nothing
flushCodeBuf linesRev =
    let lines = reverse linesRev
        contentRaw = concatMap lineTextWithEnding lines
        content = trimRight contentRaw
    in if content == ""
         then Nothing
         else let spanStart' = spanStart (plSpan (head lines))
                  spanEnd'   = spanEnd (plSpan (last lines))
                  blockSpan = SourceSpan spanStart' spanEnd'
               in Just CodeBlock
                    { cbDirectives = defaultBlockDirectives
                    , cbContent = content
                    , cbSpan = blockSpan
                    }

lineTextWithEnding :: ParsedLine -> String
lineTextWithEnding ParsedLine{..} = plText ++ plEnding

startsWithBlockDirective :: String -> Bool
startsWithBlockDirective s = isPrefixOf "{//!" (dropWhile isSpace s)

parseBlockDirectiveLine :: ParsedLine -> Either String [(String, Located Bool)]
parseBlockDirectiveLine ParsedLine{..} = do
    let tline = dropWhile isSpace plText
    if not (isPrefixOf "{//!" tline)
      then Left $ "Invalid block directive line (missing {//!): " ++ plText
      else do
        let afterPrefix = trim (drop 4 tline)
            content = takeWhile (/= '}') afterPrefix
        if null (trim content)
          then Right []
          else mapM (parseKeyValue plSpan) (splitOn ',' content)
  where
    parseKeyValue :: SourceSpan -> String -> Either String (String, Located Bool)
    parseKeyValue span s =
      case break (== ':') (trim s) of
        (keyRaw, ':' : valueRaw) -> do
          boolValue <- parseBool valueRaw
          let key = trim keyRaw
          pure (key, locatedWithSpan span boolValue)
        _ -> Left $ "Invalid key:value format: " ++ s

parseBlockDirectives :: [(String, Located Bool)] -> Either String BlockDirectives
parseBlockDirectives pairs = foldM updateDirective defaultBlockDirectives pairs
  where
    updateDirective bd (key, value) = case key of
      "ownership" -> Right bd { bdOwnership = Just value }
      "dependent_types" -> Right bd { bdDependentTypes = Just value }
      "constraints" -> Right bd { bdConstraints = Just value
                                  , bdDependentTypes = Just value }
      _ -> Left $ "Unknown block directive: " ++ key

captureDirectiveBlock
  :: SourceSpan
  -> [ParsedLine]
  -> Either String ([ParsedLine], SourceSpan, [ParsedLine])
captureDirectiveBlock directiveSpan = go 0 []
  where
    go _ _ [] = Left "Unclosed directive block: missing closing '}'"
    go depth accRev (line:rest) =
      let newDepth = depth + curlyDelta (plText line)
      in if newDepth < 0
           then let blockLines = reverse accRev
                    blockSpan = computeBlockSpan directiveSpan (plSpan line) blockLines
                in Right (blockLines, blockSpan, rest)
           else go newDepth (line:accRev) rest

computeBlockSpan :: SourceSpan -> SourceSpan -> [ParsedLine] -> SourceSpan
computeBlockSpan directiveSpan closingSpan blockLines =
    case blockLines of
      [] -> SourceSpan (spanEnd directiveSpan) (spanStart closingSpan)
      _  -> SourceSpan (spanStart (plSpan (head blockLines)))
                       (spanEnd (plSpan (last blockLines)))

buildBlockContent :: [ParsedLine] -> String
buildBlockContent blockLines =
    let texts = map plText blockLines
    in unlines texts

-- ============================================================================
-- Directive helpers
-- ============================================================================

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

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim str = case break (== delim) str of
    (before, []) -> [before]
    (before, _ : after) -> before : splitOn delim after

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- Compute net curly-brace delta for a line, ignoring braces inside strings and line-comments.
curlyDelta :: String -> Int
curlyDelta = go False False 0
  where
    go :: Bool -> Bool -> Int -> String -> Int
    go _ _ acc [] = acc
    go inStr _ acc ('/' : '/' : _) | not inStr = acc
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
