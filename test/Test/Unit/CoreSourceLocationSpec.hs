module Test.Unit.CoreSourceLocationSpec where
import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, oneof)
import Control.Monad.State 
import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..), choose)
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
                                              arbitrary = do
              line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
                                              arbitrary = do
              start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end

-- | Core functionality tests for SourceLocation module Test.Unit.CoreSourceLocationSpec :: TestTree
tests =
    testGroup "Core SourceLocation Tests"
    [ testGroup "SourcePos operations"
        [             testCase "startPos has correct initial values" $ do
                        posLine startPos @?= 1
            posColumn startPos @?= 1
            posOffset startPos @?= 0

          ,             testCase "posAfter handles different characters correctly" $ do
                        let pos1 = startPos
            posAfter 'a' pos1 @?= SourcePos 1 2 1
            posAfter '\n' pos1 @?= SourcePos 2 1 1
            posAfter '\t' pos1 @?= SourcePos 1 9 1  -- Tab moves to next 8-column boundary

          ,             testCase "posAt creates positions correctly" $ do
                        let pos = posAt 5 10
            posLine pos @?= 5
            posColumn pos @?= 10
            posOffset pos @?= 0

          ,             testCase "posAtLineCol creates positions with offset" $ do
                        let pos = posAtLineCol 3 7 42
            posLine pos @?= 3
            posColumn pos @?= 7
            posOffset pos @?= 42
        ]

    , testGroup "SourceSpan operations"
        [             testCase "emptySpan creates zero-L.length span" $ do
                        let pos = posAt 2 3
                                              span = emptySpan pos
            spanStart span @?= pos
            spanEnd span @?= pos

          ,             testCase "spanFrom creates span starting at position" $ do
                        let pos = posAt 1 1
                                              span = spanFrom pos
            spanStart span @?= pos
            spanEnd span @?= pos

          ,             testCase "spanBetween creates correct span" $ do
                        let start = posAt 1 5
                                              end = posAt 2 10
                                              span = spanBetween start end
            spanStart span @?= start
            spanEnd span @?= end

          ,             testCase "mergeSpans combines spans correctly" $ do
                        let span1 = spanBetween (posAt 1 1) (posAt 1 10)
                                              span2 = spanBetween (posAt 2 5) (posAt 3 15)
                                              merged = mergeSpans span1 span2
            spanStart merged @?= spanStart span1
            spanEnd merged @?= spanEnd span2

          ,             testCase "isValidSpan checks span validity" $ do
                        let validSpan = spanBetween (posAt 1 1) (posAt 1 10)
                                              invalidSpan = spanBetween (posAt 2 10) (posAt 1 5)
            assertBool "valid span should be valid" $ isValidSpan validSpan
            assertBool "invalid span should not be valid" $ not (isValidSpan invalidSpan)
        ]

    , testGroup "Located values"
        [             testCase "locatedAt creates located value correctly" $ do
                        let pos = posAt 3 7
                                              value = "test"
                                              located = locatedAt pos value
            locValue located @?= value
            locPos located @?= pos
            spanStart (locSpan located) @?= pos
            spanEnd (locSpan located) @?= pos

          ,             testCase "locatedWithSpan creates located value with span" $ do
                        let span = spanBetween (posAt 1 1) (posAt 1 5)
                                              value = 42
                                              located = locatedWithSpan span value
            locValue located @?= value
            locSpan located @?= span
            locPos located @?= spanStart span

          ,             testCase "mapLocated transforms value correctly" $ do
                        let pos = posAt 1 1
                                              original = locatedAt pos "hello"
                                              transformed = mapLocated L.length original
            locValue transformed @?= 5
            locPos transformed @?= pos
        ]

    , testGroup "Location tracking operations"
        [             testCase "runLocationTracker executes with start position" $ do
                        let result = runLocationTracker getCurrentPos
            result @?= startPos

          ,             testCase "position advancement works correctly" $ do
                        let pos1 = startPos
                                              pos2 = advancePos 'a' pos1
                                              pos3 = advancePos '\n' pos2
            posColumn pos2 @?= 2
            posLine pos3 @?= 2
            posColumn pos3 @?= 1

          ,             testCase "advancePosBy handles multiple characters" $ do
                        let pos1 = startPos
                                              pos2 = advancePosBy "hello" pos1
                                              pos3 = advancePosBy "world\n" pos2
            posColumn pos2 @?= 6
            posLine pos3 @?= 2
            posColumn pos3 @?= 6

          ,             testCase "withLocationTracking tracks position changes" $ do
                        let start = posAt 1 1
                (result, endPos) = withLocationTracking start $ do
                                setCurrentPos (posAt 2 5)
                    getCurrentPos
            result @?= posAt 2 5
            endPos @?= posAt 2 5
        ]

    , testGroup "Error location conversion"
        [             testCase "toErrorLocation converts position correctly" $ do
                        let pos = posAt 5 10
                                              errLoc = toErrorLocation pos
            line errLoc @?= 5
            column errLoc @?= 10
            endLine errLoc @?= Nothing
            endColumn errLoc @?= Nothing

          ,             testCase "toErrorLocationWithSpan converts span correctly" $ do
                        let span = spanBetween (posAt 3 5) (posAt 4 10)
                                              errLoc = toErrorLocationWithSpan span
            line errLoc @?= 3
            column errLoc @?= 5
            endLine errLoc @?= Just 4
            endColumn errLoc @?= Just 10
        ]

    , testGroup "Property-based tests"
        [             testProperty "posAfter increases offset by 1" $
            \pos char -> posOffset (posAfter char pos) == posOffset pos + 1

        ,             testProperty "mergeSpans is commutative in terms of coverage" $
            \span1 span2 -> let merged1 = mergeSpans span1 span2
                                                              merged2 = mergeSpans span2 span1
                            in spanStart                               merged1 == spanStart merged2 && 
                               spanEnd                               merged1 == spanEnd merged2

        ,             testProperty "locatedValue extraction is inverse of locatedAt" $
            \(pos :: SourcePos) (value :: Int) -> locatedValue (locatedAt pos value) == value

        ,             testProperty "spanFrom                               pos = emptySpan pos" $
            \pos -> spanFrom                               pos == emptySpan pos
        ]
    ]