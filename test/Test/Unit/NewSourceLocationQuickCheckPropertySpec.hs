{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | QuickCheck property tests for SourceLocation module
module Test.Unit.NewSourceLocationQuickCheckPropertySpec where

import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import SourceLocation
import Compiler.Errors.Core (ErrorLocation(..))
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.List (sort)
import Control.Monad.State (runState)

-- | Test group for SourceLocation module QuickCheck properties
testSourceLocationQuickCheckProperties :: TestTree
testSourceLocationQuickCheckProperties = testGroup "SourceLocation Module QuickCheck Property Tests"
  [ sourcePosProperties
  , sourceSpanProperties
  , locatedProperties
  , positionAdvancementProperties
  , locationTrackerProperties
  , errorLocationProperties
  ]

-- | Properties for SourcePos
sourcePosProperties :: TestTree
sourcePosProperties = testGroup "SourcePos properties"
  [ testProperty "startPos has line 1, column 1, offset 0" $
    \_ -> startPos === SourcePos 1 1 0
  
  , testProperty "posAfter newline increments line L.and resets column" $
    \pos -> posAfter '\n' pos === SourcePos (posLine pos + 1) 1 (posOffset pos + 1)
  
  , testProperty "posAfter tab aligns to next 8-column boundary" $
    \pos -> let newPos = posAfter '\t' pos
                expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
            in posColumn newPos === expectedCol
  
  , testProperty "posAfter regular char increments column L.and offset" $
    \pos c -> c /= '\n' && c /= '\t' ==> 
      posAfter c pos === SourcePos (posLine pos) (posColumn pos + 1) (posOffset pos + 1)
  
  , testProperty "posAt creates position with given line L.and column" $
    \line col -> line > 0 && col > 0 ==> 
      posAt line col === SourcePos line col 0
  
  , testProperty "posAtLineCol creates position with given line, column, L.and offset" $
    \line col offset -> line > 0 && col > 0 && offset >= 0 ==> 
      posAtLineCol line col offset === SourcePos line col offset
  ]

-- | Properties for SourceSpan
sourceSpanProperties :: TestTree
sourceSpanProperties = testGroup "SourceSpan properties"
  [ testProperty "emptySpan creates span with same start L.and end" $
    \pos -> let span = emptySpan pos
            in spanStart span === pos && spanEnd span === pos
  
  , testProperty "spanFrom equals emptySpan" $
    \pos -> spanFrom pos === emptySpan pos
  
  , testProperty "spanTo creates span with same start L.and end" $
    \pos -> let span = spanTo pos
            in spanStart span === pos && spanEnd span === pos
  
  , testProperty "spanBetween creates span with given start L.and end" $
    \start end -> spanBetween start end === SourceSpan start end
  
  , testProperty "mergeSpans creates span covering both spans" $
    \span1 span2 -> 
      let merged = mergeSpans span1 span2
      in spanStart merged === min (spanStart span1) (spanStart span2) &&
         spanEnd merged === max (spanEnd span1) (spanEnd span2)
  
  , testProperty "mergeSpans is commutative" $
    \span1 span2 -> mergeSpans span1 span2 === mergeSpans span2 span1
  
  , testProperty "mergeSpans is associative" $
    \span1 span2 span3 -> 
      mergeSpans span1 (mergeSpans span2 span3) === mergeSpans (mergeSpans span1 span2) span3
  
  , testProperty "isValidSpan checks start <= end" $
    \start end -> let span = spanBetween start end
                  in isValidSpan span === (start <= end)
  ]

-- | Properties for Located values
locatedProperties :: TestTree
locatedProperties = testGroup "Located properties"
  [ testProperty "locatedAt creates located value with empty span" $
    \pos value -> 
      let located = locatedAt pos value
      in locValue located === value &&
         locPos located === pos &&
         locSpan located === emptySpan pos
  
  , testProperty "locatedWithSpan creates located value with given span" $
    \span value -> 
      let located = locatedWithSpan span value
      in locValue located === value &&
         locPos located === spanStart span &&
         locSpan located === span
  
  , testProperty "locatedValue extracts the value" $
    \span value -> locatedValue (locatedWithSpan span value) === value
  
  , testProperty "locatedSpan extracts the span" $
    \span value -> locatedSpan (locatedWithSpan span value) === span
  
  , testProperty "locatedPos extracts the start position" $
    \span value -> locatedPos (locatedWithSpan span value) === spanStart span
  
  , testProperty "mapLocated applies function to value" $
    \span value f -> 
      let located = locatedWithSpan span value
          mapped = mapLocated f located
      in locValue mapped === f value &&
         locPos mapped === locPos located &&
         locSpan mapped === locSpan located
  
  , testProperty "mapLocated preserves location" $
    \span value f -> 
      let located = locatedWithSpan span value
          mapped = mapLocated f located
      in locPos mapped === locPos located && locSpan mapped === locSpan located
  ]

-- | Properties for position advancement
positionAdvancementProperties :: TestTree
positionAdvancementProperties = testGroup "Position advancement properties"
  [ testProperty "advancePos equals posAfter" $
    \c pos -> advancePos c pos === posAfter c pos
  
  , testProperty "advancePosBy with empty string returns same position" $
    \pos -> advancePosBy "" pos === pos
  
  , testProperty "advancePosBy is consistent with repeated advancePos" $
    \chars pos -> 
      let result1 = advancePosBy chars pos
          result2 = L.foldl (flip advancePos) pos chars
      in result1 === result2
  
  , testProperty "advancePosByText equals advancePosBy on unpacked text" $
    \text pos -> advancePosByText text pos === advancePosBy (T.unpack text) pos
  
  , testProperty "advancePosByLine increments line L.and resets column" $
    \pos numLines -> numLines > 0 ==> 
      let result = advancePosByLine numLines pos
      in posLine result === posLine pos + numLines &&
         posColumn result === 1
  
  , testProperty "advancePosByLine with 1 line equals _advanceLine" $
    \pos -> advancePosByLine 1 pos === _advanceLine pos
  ]

-- | Properties for LocationTracker
locationTrackerProperties :: TestTree
locationTrackerProperties = testGroup "LocationTracker properties"
  [ testProperty "runLocationTracker starts at startPos" $
    \_ -> runLocationTracker getCurrentPos === startPos
  
  , testProperty "setCurrentPos changes current position" $
    \pos -> runLocationTracker (setCurrentPos pos >> getCurrentPos) === pos
  
  , testProperty "markSpanStart returns current position" $
    \pos -> runLocationTracker (setCurrentPos pos >> markSpanStart) === pos
  
  , testProperty "markSpanEnd creates span from start to current" $
    \start end -> 
      let (span, _) = runState (markSpanEnd end) start
      in spanStart span === start && spanEnd span === end
  
  , testProperty "withLocationTracking runs action with given start position" $
    \start -> 
      let (result, finalPos) = withLocationTracking start getCurrentPos
      in result === start && finalPos === start
  ]

-- | Properties for error location conversion
errorLocationProperties :: TestTree
errorLocationProperties = testGroup "Error location conversion properties"
  [ testProperty "toErrorLocation converts position to error location" $
    \pos -> 
      let errLoc = toErrorLocation pos
      in filePath errLoc === Nothing &&
         line errLoc === posLine pos &&
         column errLoc === posColumn pos &&
         endLine errLoc === Nothing &&
         endColumn errLoc === Nothing
  
  , testProperty "toErrorLocationWithSpan converts span to error location with range" $
    \span -> 
      let errLoc = toErrorLocationWithSpan span
          start = spanStart span
          end = spanEnd span
      in filePath errLoc === Nothing &&
         line errLoc === posLine start &&
         column errLoc === posColumn start &&
         endLine errLoc === Just (posLine end) &&
         endColumn errLoc === Just (posColumn end)
  
  , testProperty "_toErrorLocationWithFile includes file path" $
    \mfile span -> 
      let errLoc = _toErrorLocationWithFile mfile span
          start = spanStart span
          end = spanEnd span
      in filePath errLoc === mfile &&
         line errLoc === posLine start &&
         column errLoc === posColumn start &&
         endLine errLoc === Just (posLine end) &&
         endColumn errLoc === Just (posColumn end)
  
  , testProperty "_toErrorLocationPosWithFile includes file path" $
    \mfile pos -> 
      let errLoc = _toErrorLocationPosWithFile mfile pos
      in filePath errLoc === mfile &&
         line errLoc === posLine pos &&
         column errLoc === posColumn pos &&
         endLine errLoc === Nothing &&
         endColumn errLoc === Nothing
  ]

-- Additional utility properties
additionalSourceLocationProperties :: TestTree
additionalSourceLocationProperties = testGroup "Additional SourceLocation properties"
  [ testProperty "comparePos compares by offset" $
    \pos1 pos2 -> comparePos pos1 pos2 === compare (posOffset pos1) (posOffset pos2)
  
  , testProperty "_spanLength calculates difference in offsets" $
    \span -> _spanLength span === posOffset (spanEnd span) - posOffset (spanStart span)
  
  , testProperty "_spanCovering creates span that includes both positions" $
    \pos1 pos2 -> 
      let span = _spanCovering pos1 pos2
      in _spanContains span pos1 && _spanContains span pos2
  
  , testProperty "_posDistance calculates absolute offset difference" $
    \pos1 pos2 -> _posDistance pos1 pos2 === abs (posOffset pos2 - posOffset pos1)
  
  , testProperty "_lineDistance calculates absolute line difference" $
    \pos1 pos2 -> _lineDistance pos1 pos2 === abs (posLine pos2 - posLine pos1)
  
  , testProperty "_posAtLine creates position at start of line" $
    \lineNum -> lineNum > 0 ==> 
      let pos = _posAtLine lineNum
      in posLine pos === lineNum && posColumn pos === 1 && posOffset pos === 0
  
  , testProperty "_isValidPos checks position validity" $
    \pos -> _isValidPos pos === (posLine pos > 0 && posColumn pos > 0 && posOffset pos >= 0)
  
  , testProperty "_spansOverlap is symmetric" $
    \span1 span2 -> _spansOverlap span1 span2 === _spansOverlap span2 span1
  ]