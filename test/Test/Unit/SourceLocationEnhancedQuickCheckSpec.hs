{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.SourceLocationEnhancedQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), 
                      startPos, posAfter, posAt, posAtLineCol,
                      emptySpan, spanFrom, spanTo, spanBetween, mergeSpans, isValidSpan,
                      locatedAt, locatedWithSpan, locatedValue, locatedSpan, locatedPos, mapLocated,
                      advancePos, advancePosBy, advancePosByText, advancePosByLine,
                      toErrorLocation, toErrorLocationWithSpan)
import Compiler.Errors.Core (ErrorLocation(..))
import Data.Text (Text)
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "SourceLocation Enhanced QuickCheck Tests"
  [ positionProperties
  , spanProperties
  , locatedValueProperties
  , positionAdvancementProperties
  , errorLocationProperties
  ]

-- | Source position properties
positionProperties :: TestTree
positionProperties = testGroup "Source Position Properties"
  [ testProperty "startPos is (1,1,0)" $
      posLine startPos === 1 .&&. posColumn startPos === 1 .&&. posOffset startPos === 0
  
  , testProperty "posAt creates position with correct line and column" $
      \line col -> 
        line > 0 && col > 0 ==> 
        let pos = posAt line col
        in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === 0
  
  , testProperty "posAtLineCol creates position with correct line, column, and offset" $
      \line col offset -> 
        line > 0 && col > 0 && offset >= 0 ==> 
        let pos = posAtLineCol line col offset
        in posLine pos === line .&&. posColumn pos === col .&&. posOffset pos === offset
  
  , testProperty "posAfter newline increments line and resets column" $
      \pos -> 
        let newPos = posAfter '\n' pos
        in posLine newPos === posLine pos + 1 .&&. 
           posColumn newPos === 1 .&&. 
           posOffset newPos === posOffset pos + 1
  
  , testProperty "posAfter tab advances to next tab stop" $
      \pos -> 
        let newPos = posAfter '\t' pos
            expectedCol = ((posColumn pos - 1) `div` 8 + 1) * 8 + 1
        in posColumn newPos === expectedCol .&&. 
           posOffset newPos === posOffset pos + 1
  
  , testProperty "posAfter regular character increments column and offset" $
      \pos c -> 
        c /= '\n' && c /= '\t' ==> 
        let newPos = posAfter c pos
        in posColumn newPos === posColumn pos + 1 .&&. 
           posOffset newPos === posOffset pos + 1
  ]

-- | Source span properties
spanProperties :: TestTree
spanProperties = testGroup "Source Span Properties"
  [ testProperty "emptySpan has same start and end" $
      \pos -> 
        let span = emptySpan pos
        in spanStart span === pos .&&. spanEnd span === pos
  
  , testProperty "spanFrom creates empty span at position" $
      \pos -> 
        let span = spanFrom pos
        in spanStart span === pos .&&. spanEnd span === pos
  
  , testProperty "spanTo creates empty span at position" $
      \pos -> 
        let span = spanTo pos
        in spanStart span === pos .&&. spanEnd span === pos
  
  , testProperty "spanBetween creates span with correct start and end" $
      \start end -> 
        let span = spanBetween start end
        in spanStart span === start .&&. spanEnd span === end
  
  , testProperty "mergeSpans creates span covering both spans" $
      \span1 span2 -> 
        let merged = mergeSpans span1 span2
        in spanStart merged === min (spanStart span1) (spanStart span2) .&&. 
           spanEnd merged === max (spanEnd span1) (spanEnd span2)
  
  , testProperty "isValidSpan returns true when start <= end" $
      \start end -> 
        let span = spanBetween start end
        in isValidSpan span === (start <= end)
  
  , testProperty "mergeSpans is commutative" $
      \span1 span2 -> 
        mergeSpans span1 span2 === mergeSpans span2 span1
  
  , testProperty "mergeSpans is associative" $
      \span1 span2 span3 -> 
        mergeSpans (mergeSpans span1 span2) span3 === mergeSpans span1 (mergeSpans span2 span3)
  ]

-- | Located value properties
locatedValueProperties :: TestTree
locatedValueProperties = testGroup "Located Value Properties"
  [ testProperty "locatedAt creates located value at position" $
      \pos value -> 
        let located = locatedAt pos value
        in locatedValue located === value .&&. 
           locatedPos located === pos .&&. 
           locatedSpan located === emptySpan pos
  
  , testProperty "locatedWithSpan creates located value with span" $
      \span value -> 
        let located = locatedWithSpan span value
        in locatedValue located === value .&&. 
           locatedSpan located === span .&&. 
           locatedPos located === spanStart span
  
  , testProperty "mapLocated applies function to value" $
      \span f -> 
        let located = locatedWithSpan span f
            mapped = mapLocated (+1) located
        in locatedValue mapped === f + 1 .&&. 
           locatedSpan mapped === span
  
  , testProperty "mapLocated preserves location" $
      \span value -> 
        let located = locatedWithSpan span value
            mapped = mapLocated id located
        in mapped === located
  ]

-- | Position advancement properties
positionAdvancementProperties :: TestTree
positionAdvancementProperties = testGroup "Position Advancement Properties"
  [ testProperty "advancePos equals posAfter" $
      \pos c -> advancePos c pos === posAfter c pos
  
  , testProperty "advancePosBy empty string returns original position" $
      \pos -> advancePosBy "" pos === pos
  
  , testProperty "advancePosBy is consistent with repeated advancePos" $
      \pos chars -> 
        let pos1 = advancePosBy chars pos
            pos2 = foldl (flip advancePos) pos chars
        in pos1 === pos2
  
  , testProperty "advancePosByText equals advancePosBy on unpacked text" $
      \pos text -> 
        advancePosByText text pos === advancePosBy (T.unpack text) pos
  
  , testProperty "advancePosByLine increments line and resets column" $
      \pos numLines -> 
        numLines >= 0 ==> 
        let newPos = advancePosByLine numLines pos
        in posLine newPos === posLine pos + numLines .&&. 
           posColumn newPos === 1
  
  , testProperty "advancePosByLine 0 returns original position" $
      \pos -> advancePosByLine 0 pos === pos
  ]

-- | Error location conversion properties
errorLocationProperties :: TestTree
errorLocationProperties = testGroup "Error Location Properties"
  [ testProperty "toErrorLocation converts position correctly" $
      \pos -> 
        let errLoc = toErrorLocation pos
        in line errLoc === posLine pos .&&. 
           column errLoc === posColumn pos .&&. 
           filePath errLoc === Nothing .&&. 
           endLine errLoc === Nothing .&&. 
           endColumn errLoc === Nothing
  
  , testProperty "toErrorLocationWithSpan converts span correctly" $
      \span -> 
        let errLoc = toErrorLocationWithSpan span
            start = spanStart span
            end = spanEnd span
        in line errLoc === posLine start .&&. 
           column errLoc === posColumn start .&&. 
           filePath errLoc === Nothing .&&. 
           endLine errLoc === Just (posLine end) .&&. 
           endColumn errLoc === Just (posColumn end)
  
  , testProperty "toErrorLocationWithSpan for empty span has same start and end" $
      \pos -> 
        let span = emptySpan pos
            errLoc = toErrorLocationWithSpan span
        in endLine errLoc === Just (posLine pos) .&&. 
           endColumn errLoc === Just (posColumn pos)
  ]