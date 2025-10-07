{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ParserEnhanced (
    parseTypusWithLocations,
    ParserState(..),
    ParserResult(..),
    LocatedCodeBlock(..),
    LocatedTypusFile(..),
    module Parser
) where

import Parser hiding (parseTypus)
import qualified Parser
import SourceLocation
import EnhancedErrorHandler
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State

-- ============================================================================
-- Enhanced Data Types with Location Information
-- ============================================================================

data LocatedCodeBlock = LocatedCodeBlock
    { lcbBlock :: CodeBlock
    , lcbSpan :: SourceSpan
    , lcbLineStart :: Int
    , lcbLineEnd :: Int
    } deriving (Show, Eq)

data LocatedTypusFile = LocatedTypusFile
    { ltfFile :: TypusFile
    , ltfBlocks :: [LocatedCodeBlock]
    , ltfSourceLines :: [String]  -- Original source for error reporting
    } deriving (Show, Eq)

-- Parser result with errors
data ParserResult = ParserResult
    { prFile :: Maybe LocatedTypusFile
    , prErrors :: [CompilerError]
    , prWarnings :: [CompilerError]
    } deriving (Show)

-- Parser state with location tracking
data ParserState = ParserState
    { psCurrentPos :: SourcePos
    , psCurrentLine :: Int
    , psErrors :: [CompilerError]
    , psWarnings :: [CompilerError]
    } deriving (Show)

type ParserM a = State ParserState a

-- ============================================================================
-- Main Parsing Function with Location Tracking
-- ============================================================================

-- Parse Typus file with full location tracking
parseTypusWithLocations :: String -> ParserResult
parseTypusWithLocations input = 
    let sourceLines = lines input
        initialState = ParserState
            { psCurrentPos = startPos
            , psCurrentLine = 1
            , psErrors = []
            , psWarnings = []
            }
        (result, finalState) = runState (parseWithTracking input sourceLines) initialState
    in ParserResult
        { prFile = result
        , prErrors = psErrors finalState
        , prWarnings = psWarnings finalState
        }

-- Parse with tracking
parseWithTracking :: String -> [String] -> ParserM (Maybe LocatedTypusFile)
parseWithTracking input sourceLines = do
    case Parser.parseTypus input of
        Left err -> do
            -- Convert parse error to CompilerError
            addError $ syntaxError "P001" (T.pack err) startPos
            return Nothing
        Right typusFile -> do
            -- Add location information to blocks
            locatedBlocks <- addLocationInfo (tfBlocks typusFile) sourceLines
            
            -- Validate structure
            validateStructure locatedBlocks
            
            return $ Just LocatedTypusFile
                { ltfFile = typusFile
                , ltfBlocks = locatedBlocks
                , ltfSourceLines = sourceLines
                }

-- ============================================================================
-- Location Information Addition
-- ============================================================================

-- Add location information to code blocks
addLocationInfo :: [CodeBlock] -> [String] -> ParserM [LocatedCodeBlock]
addLocationInfo blocks sourceLines = do
    let totalLines = length sourceLines
    foldM (processBlock totalLines sourceLines) [] (zip [1..] blocks)
  where
    processBlock :: Int -> [String] -> [LocatedCodeBlock] -> (Int, CodeBlock) -> ParserM [LocatedCodeBlock]
    processBlock totalLines srcLines acc (idx, block) = do
        let contentLines = lines (cbContent block)
            lineCount = length contentLines
            
        -- Estimate line positions (simplified)
        currentLine <- gets psCurrentLine
        let startLine = currentLine
            endLine = min (startLine + lineCount - 1) totalLines
            startPos = posAt startLine 1
            endPos = posAt endLine 1000  -- Approximate end column
            span = spanBetween startPos endPos
        
        -- Update current line
        modify $ \s -> s { psCurrentLine = endLine + 1 }
        
        let located = LocatedCodeBlock
                { lcbBlock = block
                , lcbSpan = span
                , lcbLineStart = startLine
                , lcbLineEnd = endLine
                }
        
        return $ acc ++ [located]

-- ============================================================================
-- Structure Validation with Location Tracking
-- ============================================================================

-- Validate code structure
validateStructure :: [LocatedCodeBlock] -> ParserM ()
validateStructure blocks = do
    -- Check for unclosed braces
    mapM_ checkBraceBalance blocks
    
    -- Check for directive consistency
    mapM_ checkDirectives blocks
    
    -- Warn about empty blocks
    mapM_ warnEmptyBlocks blocks

-- Check brace balance in a block
checkBraceBalance :: LocatedCodeBlock -> ParserM ()
checkBraceBalance LocatedCodeBlock{..} = do
    let content = cbContent lcbBlock
        openBraces = count '{' content
        closeBraces = count '}' content
    
    when (openBraces /= closeBraces) $ do
        let diff = abs (openBraces - closeBraces)
            msg = if openBraces > closeBraces
                  then T.pack $ "Unclosed braces: " ++ show diff ++ " missing closing '}'"
                  else T.pack $ "Extra closing braces: " ++ show diff ++ " extra '}'"
        addError $ syntaxError "P002" msg (spanStart lcbSpan)
  where
    count c = length . filter (== c)

-- Check directive consistency
checkDirectives :: LocatedCodeBlock -> ParserM ()
checkDirectives LocatedCodeBlock{..} = do
    let directives = cbDirectives lcbBlock
    
    -- Warn if all directives are off
    when (not (bdOwnership directives) && 
          not (bdDependentTypes directives) && 
          not (bdConstraints directives)) $ do
        addWarning $ CompilerError
            { ceError = warningWithCategory "P003" Parsing
                "Block has all directives disabled"
                (toErrorLocationWithSpan lcbSpan)
            , ceSourceContext = Nothing
            , ceStackTrace = []
            , cePhase = ParsingPhase
            }

-- Warn about empty blocks
warnEmptyBlocks :: LocatedCodeBlock -> ParserM ()
warnEmptyBlocks LocatedCodeBlock{..} = do
    let content = trim (cbContent lcbBlock)
    
    when (null content) $ do
        addWarning $ CompilerError
            { ceError = warningWithCategory "P004" Parsing
                "Empty code block"
                (toErrorLocationWithSpan lcbSpan)
            , ceSourceContext = Nothing
            , ceStackTrace = []
            , cePhase = ParsingPhase
            }

-- ============================================================================
-- Error Management
-- ============================================================================

-- Add error to parser state
addError :: CompilerError -> ParserM ()
addError err = modify $ \s -> s { psErrors = psErrors s ++ [err] }

-- Add warning to parser state
addWarning :: CompilerError -> ParserM ()
addWarning warning = modify $ \s -> s { psWarnings = psWarnings s ++ [warning] }

-- ============================================================================
-- Source Code Extraction with Location
-- ============================================================================

-- Extract source code snippet for error reporting
extractSourceSnippet :: LocatedTypusFile -> SourceSpan -> String
extractSourceSnippet LocatedTypusFile{..} span =
    let startLine = posLine (spanStart span)
        endLine = posLine (spanEnd span)
        relevantLines = take (endLine - startLine + 1) $ drop (startLine - 1) ltfSourceLines
    in unlines relevantLines

-- Extract single line with context
extractLineWithContext :: LocatedTypusFile -> Int -> (String, String, String)
extractLineWithContext LocatedTypusFile{..} lineNum =
    let prevLine = if lineNum > 1 then [ltfSourceLines !! (lineNum - 2)] else []
        currentLine = if lineNum <= length ltfSourceLines then [ltfSourceLines !! (lineNum - 1)] else []
        nextLine = if lineNum < length ltfSourceLines then [ltfSourceLines !! lineNum] else []
    in ( unlines prevLine
       , unlines currentLine
       , unlines nextLine
       )

-- Get source context for error
getErrorContext :: LocatedTypusFile -> SourceSpan -> String
getErrorContext ltf span =
    let startLine = posLine (spanStart span)
        endLine = posLine (spanEnd span)
        startCol = posColumn (spanStart span)
        endCol = posColumn (spanEnd span)
        snippet = extractSourceSnippet ltf span
        pointer = if startLine == endLine
                  then replicate (startCol - 1) ' ' ++ replicate (endCol - startCol + 1) '^'
                  else replicate (startCol - 1) ' ' ++ "^..."
    in snippet ++ pointer

-- ============================================================================
-- Enhanced Error Reporting
-- ============================================================================

-- Create detailed error with source context
createDetailedError :: String -> Text -> SourceSpan -> LocatedTypusFile -> ErrorCategory -> CompilerError
createDetailedError errId msg span ltf category =
    let context = getErrorContext ltf span
    in CompilerError
        { ceError = (errorWithCategory errId category msg (toErrorLocationWithSpan span))
            { context = emptyContext { contextCode = Just context }
            }
        , ceSourceContext = Just context
        , ceStackTrace = []
        , cePhase = ParsingPhase
        }

-- ============================================================================
-- Utility Functions
-- ============================================================================

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Get code block at specific line
getBlockAtLine :: LocatedTypusFile -> Int -> Maybe LocatedCodeBlock
getBlockAtLine LocatedTypusFile{..} lineNum =
    let matchingBlocks = filter (\lcb -> 
            lineNum >= lcbLineStart lcb && lineNum <= lcbLineEnd lcb) ltfBlocks
    in case matchingBlocks of
        [] -> Nothing
        (b:_) -> Just b

-- Find all blocks with specific directive enabled
findBlocksWithDirective :: LocatedTypusFile -> (BlockDirectives -> Bool) -> [LocatedCodeBlock]
findBlocksWithDirective LocatedTypusFile{..} predicate =
    filter (\lcb -> predicate (cbDirectives (lcbBlock lcb))) ltfBlocks

-- ============================================================================
-- Diagnostic Information
-- ============================================================================

-- Generate parsing diagnostics
generateParsingDiagnostics :: ParserResult -> String
generateParsingDiagnostics ParserResult{..} =
    let errorCount = length prErrors
        warningCount = length prWarnings
        fileInfo = case prFile of
            Nothing -> "File parsing failed"
            Just ltf -> "File parsed successfully with " ++ 
                        show (length (ltfBlocks ltf)) ++ " blocks"
    in unlines
        [ "=== Parsing Diagnostics ==="
        , fileInfo
        , "Errors: " ++ show errorCount
        , "Warnings: " ++ show warningCount
        , ""
        , if errorCount > 0
            then "Errors:\n" ++ formatCompilerErrors prErrors
            else "No errors"
        , ""
        , if warningCount > 0
            then "Warnings:\n" ++ formatCompilerErrors prWarnings
            else "No warnings"
        ]

-- Show location information for debugging
showLocationInfo :: LocatedCodeBlock -> String
showLocationInfo LocatedCodeBlock{..} =
    unlines
        [ "Block location:"
        , "  Lines: " ++ show lcbLineStart ++ "-" ++ show lcbLineEnd
        , "  Span: " ++ showSpan lcbSpan
        , "  Content preview: " ++ take 50 (cbContent lcbBlock) ++ "..."
        ]