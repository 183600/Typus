{-# LANGUAGE OverloadedStrings #-}
module SourceLocation (
    -- Source location tracking
    SourcePos(..),
    SourceSpan(..),
    Located(..),
    HasLocation(..),

    -- Position utilities
    startPos,
    posAfter,
    posAt,
    posAtLineCol,

    -- Span utilities
    emptySpan,
    spanFrom,
    spanTo,
    spanBetween,
    mergeSpans,
    isValidSpan,

    -- Located value utilities
    locatedAt,
    locatedWithSpan,
    locatedValue,
    locatedSpan,
    locatedPos,
    mapLocated,

    -- Source location tracking monad
    LocationTracker,
    runLocationTracker,
    getCurrentPos,
    setCurrentPos,
    markSpanStart,
    markSpanEnd,
    withLocationTracking,

    -- Error location conversion
    toErrorLocation,
    toErrorLocationWithSpan,

    -- Text position tracking
    advancePos,
    advancePosBy,
    advancePosByText,
    advancePosByLine
) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)
import Control.Monad.State
import ErrorHandler (ErrorLocation(..))

-- ============================================================================
-- Source Position
-- ============================================================================

data SourcePos = SourcePos
    { posLine :: Int
    , posColumn :: Int
    , posOffset :: Int
    } deriving (Show, Eq, Ord)

-- Start position (1-based)
startPos :: SourcePos
startPos = SourcePos 1 1 0

-- Position after a character
posAfter :: Char -> SourcePos -> SourcePos
posAfter '\n' pos = pos
    { posLine = posLine pos + 1
    , posColumn = 1
    , posOffset = posOffset pos + 1
    }
posAfter '\t' pos = pos
    { posColumn = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
    , posOffset = posOffset pos + 1
    }
posAfter _ pos = pos
    { posColumn = posColumn pos + 1
    , posOffset = posOffset pos + 1
    }

-- Position at specific line and column
posAt :: Int -> Int -> SourcePos
posAt line col = SourcePos line col 0

-- Position at specific line, column, and offset
posAtLineCol :: Int -> Int -> Int -> SourcePos
posAtLineCol = SourcePos

-- ============================================================================
-- Source Span
-- ============================================================================

data SourceSpan = SourceSpan
    { spanStart :: SourcePos
    , spanEnd :: SourcePos
    } deriving (Show, Eq, Ord)

-- Empty span at a position
emptySpan :: SourcePos -> SourceSpan
emptySpan pos = SourceSpan pos pos

-- Span starting at a position
spanFrom :: SourcePos -> SourceSpan
spanFrom = emptySpan

-- Span ending at a position
spanTo :: SourcePos -> SourceSpan
spanTo pos = SourceSpan pos pos

-- Span between two positions
spanBetween :: SourcePos -> SourcePos -> SourceSpan
spanBetween start end = SourceSpan start end

-- Merge two spans
mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans span1 span2 = SourceSpan
    { spanStart = min (spanStart span1) (spanStart span2)
    , spanEnd = max (spanEnd span1) (spanEnd span2)
    }

-- Check if span is valid (start <= end)
isValidSpan :: SourceSpan -> Bool
isValidSpan span = spanStart span <= spanEnd span

-- ============================================================================
-- Located Values
-- ============================================================================

data Located a = Located
    { locSpan :: SourceSpan
    , locValue :: a
    } deriving (Show, Eq, Functor)

-- Class for things that have locations
class HasLocation a where
    getLocation :: a -> SourceSpan

instance HasLocation (Located a) where
    getLocation = locSpan

-- Create located value at position
locatedAt :: SourcePos -> a -> Located a
locatedAt pos = locatedWithSpan (emptySpan pos)

-- Create located value with span
locatedWithSpan :: SourceSpan -> a -> Located a
locatedWithSpan = Located

-- Extract value from located
locatedValue :: Located a -> a
locatedValue = locValue

-- Extract span from located
locatedSpan :: Located a -> SourceSpan
locatedSpan = locSpan

-- Extract starting position from located
locatedPos :: Located a -> SourcePos
locatedPos = spanStart . locSpan

-- Map function over located value
mapLocated :: (a -> b) -> Located a -> Located b
mapLocated f loc = loc { locValue = f (locValue loc) }

-- ============================================================================
-- Location Tracking Monad
-- ============================================================================

type LocationTracker = State SourcePos

-- Run location tracker
runLocationTracker :: LocationTracker a -> a
runLocationTracker = evalState startPos

-- Get current position
getCurrentPos :: LocationTracker SourcePos
getCurrentPos = get

-- Set current position
setCurrentPos :: SourcePos -> LocationTracker ()
setCurrentPos = put

-- Mark span start (returns current position)
markSpanStart :: LocationTracker SourcePos
markSpanStart = getCurrentPos

-- Mark span end (creates span from start to current)
markSpanEnd :: SourcePos -> LocationTracker SourceSpan
markSpanEnd start = do
    end <- getCurrentPos
    return $ spanBetween start end

-- Run action with location tracking
withLocationTracking :: SourcePos -> LocationTracker a -> (a, SourcePos)
withLocationTracking start action = runState action start

-- ============================================================================
-- Position Advancement
-- ============================================================================

-- Advance position by one character
advancePos :: Char -> SourcePos -> SourcePos
advancePos = posAfter

-- Advance position by multiple characters
advancePosBy :: String -> SourcePos -> SourcePos
advancePosBy = foldl (flip advancePos)

-- Advance position by text
advancePosByText :: Text -> SourcePos -> SourcePos
advancePosByText text = advancePosBy (T.unpack text)

-- Advance position by line
advancePosByLine :: Int -> SourcePos -> SourcePos
advancePosByLine lines pos = pos
    { posLine = posLine pos + lines
    , posColumn = 1
    }

-- ============================================================================
-- Error Location Conversion
-- ============================================================================

-- Convert source position to error location
toErrorLocation :: SourcePos -> ErrorLocation
toErrorLocation pos = ErrorLocation
    { filePath = Nothing
    , line = posLine pos
    , column = posColumn pos
    , endLine = Nothing
    , endColumn = Nothing
    }

-- Convert source span to error location with range
toErrorLocationWithSpan :: SourceSpan -> ErrorLocation
toErrorLocationWithSpan span = ErrorLocation
    { filePath = Nothing
    , line = posLine (spanStart span)
    , column = posColumn (spanStart span)
    , endLine = Just (posLine (spanEnd span))
    , endColumn = Just (posColumn (spanEnd span))
    }

-- ============================================================================
-- Utilities
-- ============================================================================

-- Compare positions
comparePos :: SourcePos -> SourcePos -> Ordering
comparePos p1 p2 = compare (posOffset p1) (posOffset p2)

-- Check if position is within span
isPosInSpan :: SourcePos -> SourceSpan -> Bool
isPosInSpan pos span = pos >= spanStart span && pos <= spanEnd span

-- Check if two spans overlap
doSpansOverlap :: SourceSpan -> SourceSpan -> Bool
doSpansOverlap span1 span2 =
    spanStart span1 <= spanEnd span2 && spanEnd span1 >= spanStart span2

-- Get span length (in characters)
spanLength :: SourceSpan -> Int
spanLength span = posOffset (spanEnd span) - posOffset (spanStart span)

-- Show position as "line:column"
showPos :: SourcePos -> String
showPos pos = show (posLine pos) ++ ":" ++ show (posColumn pos)

-- Show span as "startLine:startCol-endLine:endCol"
showSpan :: SourceSpan -> String
showSpan span =
    showPos (spanStart span) ++ "-" ++ showPos (spanEnd span)

-- Show position with file name
showPosWithFile :: Maybe String -> SourcePos -> String
showPosWithFile mfile pos = case mfile of
    Nothing -> showPos pos
    Just file -> file ++ ":" ++ showPos pos

-- Show span with file name
showSpanWithFile :: Maybe String -> SourceSpan -> String
showSpanWithFile mfile span =
    showPosWithFile mfile (spanStart span) ++ "-" ++ showPos (spanEnd span)

-- Create position from line and column only (1-based)
posFromLineCol :: Int -> Int -> SourcePos
posFromLineCol line col = SourcePos line col 0

-- Advance position by a whole line
advanceLine :: SourcePos -> SourcePos
advanceLine pos = pos
    { posLine = posLine pos + 1
    , posColumn = 1
    , posOffset = posOffset pos + 1
    }

-- Check if position is valid
isValidPos :: SourcePos -> Bool
isValidPos pos = posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0

-- Get the minimum of two positions
minPos :: SourcePos -> SourcePos -> SourcePos
minPos p1 p2 = if comparePos p1 p2 == LT then p1 else p2

-- Get the maximum of two positions
maxPos :: SourcePos -> SourcePos -> SourcePos
maxPos p1 p2 = if comparePos p1 p2 == GT then p1 else p2

-- Create a span that covers both positions
spanCovering :: SourcePos -> SourcePos -> SourceSpan
spanCovering p1 p2 = SourceSpan (minPos p1 p2) (maxPos p1 p2)

-- Expand span by given number of characters on each side
expandSpan :: Int -> Int -> SourceSpan -> SourceSpan
expandSpan before after span =
    let start = spanStart span
        end = spanEnd span
        newStart = posFromLineCol (posLine start) (max 1 (posColumn start - before))
        newEnd = posFromLineCol (posLine end) (posColumn end + after)
    in SourceSpan newStart newEnd

-- Check if span contains a position
spanContains :: SourceSpan -> SourcePos -> Bool
spanContains span pos = pos >= spanStart span && pos <= spanEnd span

-- Check if two spans overlap
spansOverlap :: SourceSpan -> SourceSpan -> Bool
spansOverlap span1 span2 =
    spanContains span1 (spanStart span2) || spanContains span1 (spanEnd span2) ||
    spanContains span2 (spanStart span1) || spanContains span2 (spanEnd span1)

-- Merge overlapping spans
mergeOverlappingSpans :: [SourceSpan] -> [SourceSpan]
mergeOverlappingSpans = foldr merge []
  where
    merge current [] = [current]
    merge current (acc:rest)
        | spansOverlap current acc = merge (spanCovering (spanStart current) (spanEnd acc)) rest
        | otherwise = current : acc : rest

-- Calculate the distance between two positions (in characters)
posDistance :: SourcePos -> SourcePos -> Int
posDistance p1 p2 = abs (posOffset p2 - posOffset p1)

-- Get the line number difference
lineDistance :: SourcePos -> SourcePos -> Int
lineDistance p1 p2 = abs (posLine p2 - posLine p1)

-- Create position at the beginning of a given line number
posAtLine :: Int -> SourcePos
posAtLine line = SourcePos line 1 0

-- Create position at the end of a given line (approximate)
posAtLineEnd :: Int -> SourcePos
posAtLineEnd line = SourcePos line 100000 0  -- Large column number to represent end of line

-- Convert span to human-readable range description
spanToRangeDesc :: SourceSpan -> String
spanToRangeDesc span =
    let start = spanStart span
        end = spanEnd span
    in if posLine start == posLine end
       then "line " ++ show (posLine start) ++ ", columns " ++ show (posColumn start) ++ "-" ++ show (posColumn end)
       else "lines " ++ show (posLine start) ++ ":" ++ show (posColumn start) ++ " - " ++ show (posLine end) ++ ":" ++ show (posColumn end)

-- Create error location from source span with optional file
toErrorLocationWithFile :: Maybe String -> SourceSpan -> ErrorLocation
toErrorLocationWithFile mfile span = ErrorLocation
    { filePath = mfile
    , line = posLine (spanStart span)
    , column = posColumn (spanStart span)
    , endLine = Just (posLine (spanEnd span))
    , endColumn = Just (posColumn (spanEnd span))
    }

-- Create error location from source position with optional file
toErrorLocationPosWithFile :: Maybe String -> SourcePos -> ErrorLocation
toErrorLocationPosWithFile mfile pos = ErrorLocation
    { filePath = mfile
    , line = posLine pos
    , column = posColumn pos
    , endLine = Nothing
    , endColumn = Nothing
    }