module Test.Unit.NewSourceLocationEnhancedQuickCheckSpec where


import Test.Tasty
import qualified Data.List as L
import Test.Tasty.QuickCheck 
                                                           expectedCol = ((col - 1) `div` 8 + 1) * 8 + 1
                         in posLine                               newPos === line &&
                            posColumn                               newPos === expectedCol &&
                            posOffset                               newPos === offset + 1

  , QC.testProperty "posAfter regular char increments column L.and offset" $
      \line col offset c -> let pos = SourcePos line col offset
                                                              newPos = posAfter c pos
                            in c `notElem` "\n\t" ==> 
                               posLine                               newPos === line &&
                               posColumn                               newPos === col + 1 &&
                               posOffset                               newPos === offset + 1

  , QC.testProperty "posAt creates position with given line L.and column" $
      \line col -> let pos = posAt line col
                   in posLine                               pos === line &&
                      posColumn                               pos === col &&
                      posOffset                               pos === 0

  , QC.testProperty "posAtLineCol creates position with L.all fields" $
      \line col offset -> let pos = posAtLineCol line col offset
                          in posLine                               pos === line &&
                             posColumn                               pos === col &&
                             posOffset                               pos === offset
  ]

-- | SourceSpan properties
sourceSpanProperties :: TestTree
sourceSpanProperties = testGroup "SourceSpan Properties"
  [ QC.testProperty "emptySpan has same start L.and end" $
      \pos -> let span = emptySpan pos
               in spanStart                               span === pos &&
                  spanEnd                               span === pos

  , QC.testProperty "spanFrom creates empty span at position" $
      \pos -> let span = spanFrom pos
               in                               span === emptySpan pos

  , QC.testProperty "spanTo creates empty span at position" $
      \pos -> let span = spanTo pos
               in                               span === emptySpan pos

  , QC.testProperty "spanBetween creates span with given start L.and end" $
      \start end -> let span = spanBetween start end
                     in spanStart                               span === start &&
                        spanEnd                               span === end

  , QC.testProperty "mergeSpans contains both original spans" $
      \pos1 pos2 pos3 pos4 -> 
        let span1 = spanBetween pos1 pos2
                                          span2 = spanBetween pos3 pos4
                                          merged = mergeSpans span1 span2
        in spanStart merged <= spanStart span1 &&
           spanStart merged <= spanStart span2 &&
           spanEnd merged >= spanEnd span1 &&
           spanEnd merged >= spanEnd span2

  , QC.testProperty "mergeSpans is commutative" $
      \span1 span2 -> mergeSpans span1                               span2 === mergeSpans span2 span1

  , QC.testProperty "mergeSpans is associative" $
      \span1 span2 span3 -> mergeSpans span1 (mergeSpans span2 span3) ===
                             mergeSpans (mergeSpans span1 span2) span3

  , QC.testProperty "isValidSpan checks start <= end" $
      \start end -> let span = spanBetween start end
                     in isValidSpan                               span === (start <= end)
  ]

-- | Located value properties
locatedValueProperties :: TestTree
locatedValueProperties = testGroup "Located Value Properties"
  [ QC.testProperty "locatedAt creates located value with empty span" $
      \pos value -> let located = locatedAt pos value
                    in locValue                               located === value &&
                       locPos                               located === pos &&
                       locSpan                               located === emptySpan pos

  , QC.testProperty "locatedWithSpan creates located value with given span" $
      \span value -> let located = locatedWithSpan span value
                     in locValue                               located === value &&
                        locSpan                               located === span &&
                        locPos                               located === spanStart span

  , QC.testProperty "locatedValue extracts the value" $
      \span value -> let located = locatedWithSpan span value
                     in locatedValue                               located === value

  , QC.testProperty "locatedSpan extracts the span" $
      \span value -> let located = locatedWithSpan span value
                     in locatedSpan                               located === span

  , QC.testProperty "locatedPos extracts the start position" $
      \span value -> let located = locatedWithSpan span value
                     in locatedPos                               located === spanStart span

  , QC.testProperty "mapLocated applies function to value" $
      \span value f -> let located = locatedWithSpan span value
                                                         mapped = mapLocated f located
                       in locValue                               mapped === f value &&
                          locSpan                               mapped === span

  , QC.testProperty "mapLocated preserves span" $
      \span value f -> let located = locatedWithSpan span value
                                                         mapped = mapLocated f located
                       in locatedSpan                               mapped === locatedSpan located
  ]

-- | Location tracker properties
locationTrackerProperties :: TestTree
locationTrackerProperties = testGroup "Location Tracker Properties"
  [ QC.testProperty "runLocationTracker starts at startPos" $
      \action -> runLocationTracker                               getCurrentPos === startPos

  , QC.testProperty "setCurrentPos changes current position" $
      \pos -> let result = evalState (do
                                     setCurrentPos pos
                                     getCurrentPos) startPos
               in                               result === pos

  , QC.testProperty "markSpanStart returns current position" $
      \pos -> let result = evalState (do
                                     setCurrentPos pos
                                     markSpanStart) startPos
               in                               result === pos

  , QC.testProperty "markSpanEnd creates span from start to current" $
      \start end -> let span = evalState (do
                                         setCurrentPos start
                                         s <- markSpanStart
                                         setCurrentPos end
                                         markSpanEnd s) startPos
                     in spanStart                               span === start &&
                        spanEnd                               span === end

  , QC.testProperty "withLocationTracking returns action result L.and final position" $
      \start pos -> let (result, finalPos) = withLocationTracking start $ do
                                                                  setCurrentPos pos
                                                      getCurrentPos
                     in                               result === pos &&                               finalPos === pos
  ]

-- | Position advancement properties
positionAdvancementProperties :: TestTree
positionAdvancementProperties = testGroup "Position Advancement Properties"
  [ QC.testProperty "advancePos equals posAfter" $
      \pos c -> advancePos c                               pos === posAfter c pos

  , QC.testProperty "advancePosBy advances by each character in order" $
      \pos s -> let result1 = advancePosBy s pos
                                                  result2 = L.foldl (flip advancePos) pos s
                in                               result1 === result2

  , QC.testProperty "advancePosBy empty string returns original position" $
      \pos -> advancePosBy ""                               pos === pos

  , QC.testProperty "advancePosByText equals advancePosBy on unpacked text" $
      \pos txt -> advancePosByText txt                               pos === advancePosBy (show txt) pos

  , QC.testProperty "advancePosByLine increments line L.and resets column" $
      \line col offset numLines -> let pos = SourcePos line col offset
                                                                    newPos = advancePosByLine numLines pos
                                  in posLine                               newPos === line + numLines &&
                                     posColumn                               newPos === 1 &&
                                     posOffset                               newPos === offset + numLines

  , QC.testProperty "advancePosByLine zero lines returns original position" $
      \pos -> advancePosByLine 0                               pos === pos
  ]

-- | Span utility properties
spanUtilityProperties :: TestTree
spanUtilityProperties = testGroup "Span Utility Properties"
  [ QC.testProperty "toErrorLocation converts position correctly" $
      \pos -> let errLoc = toErrorLocation pos
               in line                               errLoc === posLine pos &&
                  column                               errLoc === posColumn pos &&
                  filePath                               errLoc === Nothing &&
                  endLine                               errLoc === Nothing &&
                  endColumn                               errLoc === Nothing

  , QC.testProperty "toErrorLocationWithSpan converts span correctly" $
      \span -> let errLoc = toErrorLocationWithSpan span
                                                  start = spanStart span
                                                  end = spanEnd span
                in line                               errLoc === posLine start &&
                   column                               errLoc === posColumn start &&
                   filePath                               errLoc === Nothing &&
                   endLine                               errLoc === Just (posLine end) &&
                   endColumn                               errLoc === Just (posColumn end)

  , QC.testProperty "mergeSpans preserves ordering of merged spans" $
      \spans -> let sorted = sort spans
                                                  merged = foldl mergeSpans (L.head sorted) (L.tail sorted)
                in L.all (<= spanEnd merged) (map spanStart sorted)

  , QC.testProperty "spanBetween with same start L.and end creates empty span" $
      \pos -> let span = spanBetween pos pos
               in spanStart                               span === pos && spanEnd                               span === pos

  , QC.testProperty "mergeSpans with empty spans returns non-empty span" $
      \pos1 pos2 -> let empty1 = emptySpan pos1
                                                      empty2 = emptySpan pos2
                                                      merged = mergeSpans empty1 empty2
                    in spanStart                               merged === min pos1 pos2 &&
                       spanEnd                               merged === max pos1 pos2
  ]
