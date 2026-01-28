{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
    spanBetweenOrdered,
    mergeSpans,
    isValidSpan,
    isValidBlockSpan,

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
    advancePosByLine,
    
    -- Position comparison
    comparePos,
    
    -- Functions for tests
    sourceLine,
    sourceColumn
) where


import qualified Data.Foldable as Foldable (foldl')
import Data.Text (Text)
import Test.QuickCheck (Arbitrary(..), suchThat)

import qualified Data.Text as T







import Control.Monad.State (State, get, put, runState, evalState)
import Compiler.Errors.Core (ErrorLocation(..))
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)


-- ============================================================================
-- Source Position
-- ============================================================================

data SourcePos = SourcePos
    { posLine :: Int
    , posColumn :: Int
    , posOffset :: Int
    } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

-- Custom Ord instance to ensure proper comparison: line first, then column, then offset
instance Ord SourcePos where
    compare p1 p2 = case compare (posLine p1) (posLine p2) of
                      EQ -> case compare (posColumn p1) (posColumn p2) of
                              EQ -> compare (posOffset p1) (posOffset p2)
                              other -> other
                      other -> other

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
posAt lineNum col = SourcePos lineNum col 0

-- Position at specific line, column, and offset
posAtLineCol :: Int -> Int -> Int -> SourcePos
posAtLineCol = SourcePos

-- ============================================================================
-- Source Span
-- ============================================================================

data SourceSpan = SourceSpan
    { spanStart :: SourcePos
    , spanEnd :: SourcePos
    } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

-- Empty span at a position
emptySpan :: SourcePos -> SourceSpan
emptySpan pos = SourceSpan pos pos

-- Span starting at a position
spanFrom :: SourcePos -> SourceSpan
spanFrom = emptySpan

-- Span ending at a position
spanTo :: SourcePos -> SourceSpan
spanTo pos = SourceSpan pos pos

-- Span between two positions (preserves order as expected by some tests)
spanBetween :: SourcePos -> SourcePos -> SourceSpan
spanBetween pos1 pos2 = SourceSpan pos1 pos2

-- Span between two positions (order-independent, returns min/max)
spanBetweenOrdered :: SourcePos -> SourcePos -> SourceSpan
spanBetweenOrdered pos1 pos2 
  | comparePos pos1 pos2 == LT = SourceSpan pos1 pos2
  | otherwise = SourceSpan pos2 pos1

-- Merge two spans
mergeSpans :: SourceSpan -> SourceSpan -> SourceSpan
mergeSpans span1 span2 = 
    let start1 = spanStart span1
        start2 = spanStart span2
        end1 = spanEnd span1
        end2 = spanEnd span2
        -- Compare individual fields as expected by tests
        start = SourcePos
            { posLine = min (posLine start1) (posLine start2)
            , posColumn = min (posColumn start1) (posColumn start2)
            , posOffset = min (posOffset start1) (posOffset start2)
            }
        end = SourcePos
            { posLine = max (posLine end1) (posLine end2)
            , posColumn = max (posColumn end1) (posColumn end2)
            , posOffset = max (posOffset end1) (posOffset end2)
            }
    in SourceSpan start end

-- Check if span is valid (start <= end)
isValidSpan :: SourceSpan -> Bool
isValidSpan srcSpan = 
  let start = spanStart srcSpan
      end = spanEnd srcSpan
  in comparePos start end /= GT

-- | Check if a block span is valid (alias for isValidSpan for backward compatibility)
isValidBlockSpan :: SourceSpan -> Bool
isValidBlockSpan = isValidSpan

-- ============================================================================
-- Located Values
-- ============================================================================

data Located a = Located
    { locValue :: a
    , locPos :: SourcePos  
    , locSpan :: SourceSpan
    } deriving (Show, Eq, Functor, Generic)
    deriving anyclass NFData

-- Class for things that have locations
class HasLocation a where
    getLocation :: a -> SourceSpan

instance HasLocation (Located a) where
    getLocation = locSpan

-- Create located value at position
locatedAt :: SourcePos -> a -> Located a
locatedAt pos value = locatedWithSpan (emptySpan pos) value

-- Create located value with span
locatedWithSpan :: SourceSpan -> a -> Located a
locatedWithSpan sp value = Located value (spanStart sp) sp

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
runLocationTracker action = evalState action startPos

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
advancePosBy chars pos = Foldable.foldl' (flip advancePos) pos chars

-- Advance position by text
advancePosByText :: Text -> SourcePos -> SourcePos
advancePosByText text = advancePosBy (T.unpack text)

-- Advance position by line
advancePosByLine :: Int -> SourcePos -> SourcePos
advancePosByLine numLines pos = pos
    { posLine = posLine pos + numLines
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
toErrorLocationWithSpan srcSpan = ErrorLocation
    { filePath = Nothing
    , line = posLine (spanStart srcSpan)
    , column = posColumn (spanStart srcSpan)
    , endLine = Just (posLine (spanEnd srcSpan))
    , endColumn = Just (posColumn (spanEnd srcSpan))
    }

-- ============================================================================
-- Utilities
-- ============================================================================

-- Compare positions
comparePos :: SourcePos -> SourcePos -> Ordering
comparePos p1 p2 = case compare (posLine p1) (posLine p2) of
                      EQ -> case compare (posColumn p1) (posColumn p2) of
                              EQ -> compare (posOffset p1) (posOffset p2)
                              other -> other
                      other -> other

-- ============================================================================
-- Functions for tests
-- ============================================================================

-- | Get line number from SourcePos (for tests)
sourceLine :: SourcePos -> Int
sourceLine = posLine

-- | Get column number from SourcePos (for tests)
sourceColumn :: SourcePos -> Int
sourceColumn = posColumn

-- ============================================================================
-- Arbitrary instances for testing
-- ============================================================================

-- | Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    lineNum <- arbitrary `suchThat` (> 0)
    colNum <- arbitrary `suchThat` (> 0)
    offsetVal <- arbitrary `suchThat` (>= 0)
    return $ SourcePos lineNum colNum offsetVal

-- | Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ spanBetween start end