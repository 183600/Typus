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

import Control.Applicative (empty)
import Control.Monad (foldM)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import qualified Data.Text as T
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
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L
import Utils (trim)
import qualified SyntaxValidator

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

type DirectiveParser = MP.Parsec Void T.Text

directiveSpace :: DirectiveParser ()
directiveSpace = L.space MC.space1 empty empty

lexeme :: DirectiveParser a -> DirectiveParser a
lexeme = L.lexeme directiveSpace

symbol :: String -> DirectiveParser T.Text
symbol = L.symbol directiveSpace . T.pack

identifier :: DirectiveParser T.Text
identifier = lexeme (T.pack <$> MP.some (MP.satisfy isIdentifierChar))

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_' || c == '-'

fileDirectiveParser :: DirectiveParser [(T.Text, T.Text)]
fileDirectiveParser = do
  _ <- symbol "//!"
  pairs <- MP.sepBy directive (symbol ",")
  pure pairs
  where
    directive = do
      key <- identifier
      _ <- symbol ":"
      value <- identifier
      pure (key, value)

blockDirectiveParser :: DirectiveParser [(T.Text, T.Text)]
blockDirectiveParser = do
  _ <- symbol "{//!"
  pairs <- MP.sepBy directive (symbol ",")
  _ <- symbol "}"
  pure pairs
  where
    directive = do
      key <- identifier
      _ <- symbol ":"
      value <- identifier
      pure (key, value)

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
    lineEnding <- MP.optional . MP.try $ MP.chunk "\r\n"
    ending <- case lineEnding of
      Just crlf -> pure crlf
      Nothing   -> fromMaybe "" <$> MP.optional (MP.chunk "\n" MP.<|> MP.chunk "\r")
    endPos <- MP.getSourcePos
    endOffset <- MP.getOffset
    let srcSpan = SourceSpan (toSourcePos startPos startOffset) (toSourcePos endPos endOffset)
    pure ParsedLine
      { plText = content
      , plEnding = ending
      , plSpan = srcSpan
      }

-- Convert Megaparsec SourcePos to project SourcePos
toSourcePos :: MP.SourcePos -> Int -> SourcePos
toSourcePos pos offset = SourcePos
    { posLine = MP.unPos (MP.sourceLine pos)
    , posColumn = MP.unPos (MP.sourceColumn pos)
    , posOffset = offset
    }

-- ============================================================================
-- High-level assembly
-- ============================================================================

buildTypusFile :: [ParsedLine] -> Either String TypusFile
buildTypusFile lines0 = do
    -- Check for multiple package declarations
    checkMultiplePackageDeclarations lines0
    -- Check for syntax errors
    let content = unlines (map plText lines0)
        syntaxErrors = SyntaxValidator.validateSyntax content
        -- Only report critical syntax errors (like unclosed braces)
        criticalErrors = filter (\e -> SyntaxValidator.errorType e == SyntaxValidator.MissingBrace) syntaxErrors
    if not (null criticalErrors)
       then case head criticalErrors of
                SyntaxValidator.SyntaxError{..} -> 
                  if errorMessage == "Unclosed {"
                     then Left "Unclosed directive block"
                     else Left $ "Syntax error: " ++ show (head criticalErrors)
       else do
         (fileDirs, buildTags, rest) <- parseFileDirectivesFromParsedLines lines0
         blocks <- parseBlocksFromParsedLines rest
         pure TypusFile
           { tfDirectives = fileDirs
           , tfBuildTags = buildTags
           , tfBlocks = blocks
           }

-- Check for multiple package declarations
checkMultiplePackageDeclarations :: [ParsedLine] -> Either String ()
checkMultiplePackageDeclarations lines' = do
    let packageLines = filter (isPackageDeclaration . plText) lines'
    if length packageLines > 1
        then Left "Multiple package declarations found"
        else Right ()
  where
    isPackageDeclaration line = 
        let trimmed = trim line
            wordsList = words trimmed
        in case wordsList of
            ("package":_) -> not (isPrefixOf "//" trimmed)
            _ -> False

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
               directives <- parseFileDirectiveLine line
               acc' <- foldM (\fd (key, val) -> updateFileDirective fd key val) acc directives
               go acc' buildTagsRev rest
           else if isBuildTagLine trimmed
             then let tag = locatedWithSpan (plSpan line) (plText line)
                  in go acc (tag : buildTagsRev) rest
             else Right (acc, reverse buildTagsRev, line:rest)

    isBuildTagLine t =
      isPrefixOf "//go:build" t || isPrefixOf "// +build" t

parseFileDirectiveLine :: ParsedLine -> Either String [(String, Located Bool)]
parseFileDirectiveLine ParsedLine{..} = do
    let stripped = T.stripStart (T.pack plText)
        filePrefix = T.pack "//!"
    if not (filePrefix `T.isPrefixOf` stripped)
      then Left $ "Invalid file directive format: " ++ plText
      else
        case MP.runParser (fileDirectiveParser <* MP.eof) "<file directive>" stripped of
          Left _ -> Left $ "Invalid file directive format: " ++ plText
          Right pairs -> do
            let boolPairs = map (\(keyText, valueText) ->
                  case parseBool (T.unpack valueText) of
                    Left err -> Left err
                    Right boolVal -> Right (T.unpack keyText, locatedWithSpan plSpan boolVal)
                  ) pairs
            sequence boolPairs

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
             (blockLines, blockSpan, remaining) <- captureDirectiveBlock line rest
             let content = buildBlockContent blockLines
                 block = CodeBlock
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
    case reverse linesRev of
      [] -> Nothing
      firstLine : restLines ->
        let forwardLines = firstLine : restLines
            contentRaw = concatMap lineTextWithEnding forwardLines
            content = trimRight contentRaw
            lastLine = foldLast firstLine restLines
        in if null content
             then Nothing
             else let spanStart' = spanStart (plSpan firstLine)
                      spanEnd'   = spanEnd (plSpan lastLine)
                      blockSpan = SourceSpan spanStart' spanEnd'
                  in Just CodeBlock
                        { cbDirectives = defaultBlockDirectives
                        , cbContent = content
                        , cbSpan = blockSpan
                        }
  where
    foldLast current [] = current
    foldLast _ (x:xs) = foldLast x xs

lineTextWithEnding :: ParsedLine -> String
lineTextWithEnding ParsedLine{..} = plText ++ plEnding

startsWithBlockDirective :: String -> Bool
startsWithBlockDirective = T.isPrefixOf (T.pack "{//!") . T.stripStart . T.pack

parseBlockDirectiveLine :: ParsedLine -> Either String [(String, Located Bool)]
parseBlockDirectiveLine ParsedLine{..} = do
    let stripped = T.stripStart (T.pack plText)
        blockPrefix = T.pack "{//!"
    if not (blockPrefix `T.isPrefixOf` stripped)
      then Left $ "Invalid block directive line (missing {//!): " ++ plText
      else
        let normalized = ensureClosingBrace stripped
        in case MP.runParser (blockDirectiveParser <* MP.eof) "<block directives>" normalized of
             Left _ -> Left $ "Invalid block directive line: " ++ plText
             Right pairs -> mapM convert pairs
  where
    convert (keyText, valueText) =
      case parseBool (T.unpack valueText) of
        Left err      -> Left err
        Right boolVal -> Right (T.unpack keyText, locatedWithSpan plSpan boolVal)
    ensureClosingBrace txt =
      let trimmedEnd = T.dropWhileEnd isSpace txt
      in if closingBrace `T.isSuffixOf` trimmedEnd
           then trimmedEnd
           else trimmedEnd <> closingBrace
    closingBrace = T.singleton '}'

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
  :: ParsedLine
  -> [ParsedLine]
  -> Either String ([ParsedLine], SourceSpan, [ParsedLine])
captureDirectiveBlock directiveLine = go 0 []
  where
    directiveIndent = leadingIndentation (plText directiveLine)
    directiveSpan = plSpan directiveLine

    go _ _ [] = Left "Unclosed directive block: missing closing '}'"
    go depth accRev (line:rest) =
      let newDepth = depth + curlyDelta (plText line)
      in if newDepth < 0
           then
             let closingIndent = leadingIndentation (plText line)
             in if closingIndent < directiveIndent
                  then Left "Unclosed directive block: missing closing '}'"
                  else
                    let blockLines = reverse accRev
                        blockSpan = computeBlockSpan directiveSpan (plSpan line) blockLines
                    in Right (blockLines, blockSpan, rest)
           else go newDepth (line:accRev) rest

computeBlockSpan :: SourceSpan -> SourceSpan -> [ParsedLine] -> SourceSpan
computeBlockSpan directiveSpan closingSpan blockLines =
    case blockLines of
      [] -> SourceSpan (spanEnd directiveSpan) (spanStart closingSpan)
      (firstLine:restLines) ->
        let lastLine = foldLastLine firstLine restLines
        in SourceSpan (spanStart (plSpan firstLine))
                      (spanEnd (plSpan lastLine))
  where
    foldLastLine current [] = current
    foldLastLine _ (x:xs) = foldLastLine x xs

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
    v -> Left $ "Invalid boolean value for directive: " ++ v

-- ============================================================================
-- Utility Functions
-- ============================================================================

trimRight :: String -> String
trimRight = T.unpack . T.dropWhileEnd (`elem` ['\r', '\n']) . T.pack

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

leadingIndentation :: String -> Int
leadingIndentation = length . takeWhile isIndentChar
  where
    isIndentChar c = c == ' ' || c == '\t'
