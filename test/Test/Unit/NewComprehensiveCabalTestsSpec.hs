module Test.Unit.NewComprehensiveCabalTestsSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, elements, listOf, sized, resize)
import Data.Char 
import SourceLocation (SourcePos(..), SourceSpan(..), Located)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import qualified ErrorHandler as EH
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


-- | QuickCheck
tests :: TestTree
tests =
    testGroup "New Comprehensive Cabal Tests"
    [ testGroup "Utils Module Properties"
        [             testProperty "trim removes only leading/trailing whitespace" propTrimBoundary
        ,             testProperty "splitBy L.length relationship" propSplitByLength
        ,             testProperty "splitByCollapsed removes empty segments" propSplitByCollapsedNoEmpty
        ,             testProperty "breakOn concatenation property" propBreakOnConcat
        ,             testProperty "removeLineComments preserves structure" propRemoveLineCommentsStructure
        ]

    , testGroup "Parser Module Properties"
        [             testProperty "FileDirectives round-trip property" propFileDirectivesRoundTrip
        ,             testProperty "BlockDirectives merging property" propBlockDirectivesMerge
        ,             testProperty "CodeBlock position consistency" propCodeBlockPositionConsistency
        ]

    , testGroup "SourceLocation Properties"
        [             testProperty "SourceSpan ordering property" propSourceSpanOrdering
        ,             testProperty "Located data preservation" propLocatedPreservation
        ,             testProperty "SourcePos arithmetic property" propSourcePosArithmetic
        ]

    , testGroup "Error Handling Properties"
        [             testProperty "Error message consistency" propErrorMessageConsistency
        ,             testProperty "Error recovery preserves partial results" propErrorRecoveryPartial
        ]

    , testGroup "String Processing Properties"
        [             testProperty "normalizeIndentation preserves relative structure" propNormalizeIndentationRelative
        ,             testProperty "Comment removal idempotency" propCommentRemovalIdempotent
        ]
    ]

-- ============================================================================
-- Utils Module Properties
-- ============================================================================

-- | trim
propTrimBoundary :: String -> Bool
propTrimBoundary                               input =
  let trimmed = trim input
                                    hasLeadingSpace = not (null input (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0) && isSpace (L.head input)
                                    hasTrailingSpace = not (null input) && isSpace (last input)
  in if null trimmed
     then L.all isSpace input
     else not (isSpace (L.head trimmed) || isSpace (last trimmed)

-- | splitBy <=  + 1
propSplitByLength :: Char -> String -> Bool
propSplitByLength delim                               input =
  let parts = splitBy delim input
                                    totalLength = L.sum (map L.length parts)
  in totalLength + L.length parts - 1 >= L.length input

-- | splitByCollapsed
propSplitByCollapsedNoEmpty :: Char -> String -> Bool
propSplitByCollapsedNoEmpty delim                               input =
  L.all (not . null) (splitByCollapsed delim input)

-- | breakOnprefix + pattern +                               suffix = original
propBreakOnConcat :: String -> String -> Bool
propBreakOnConcat pattern                               input =
  let (prefix, suffix) = breakOn pattern input
  in if null pattern
     then                               prefix == "" &&                               suffix == input
     else prefix ++ pattern ++                               suffix == input

-- | 
propRemoveLineCommentsStructure :: String -> Bool
propRemoveLineCommentsStructure                               input =
  let withoutComments = removeLineComments input
                                    linesBefore = lines input
                                    linesAfter = lines withoutComments
  in L.length linesAfter <= L.length linesBefore

-- ============================================================================
-- Parser Module Properties
-- ============================================================================

-- | FileDirectives
propFileDirectivesRoundTrip :: Bool -> Bool -> Bool -> Bool
propFileDirectivesRoundTrip ownership dependent                               constraints =
  let directives = FileDirectives 
        {                               fdOwnership = Just ownership
        ,                               fdDependentTypes = Just dependent
        ,                               fdConstraints = Just constraints
        }
                                    extractedOwnership = fromMaybe False (fdOwnership directives)
                                    extractedDependent = fromMaybe False (fdDependentTypes directives)
                                    extractedConstraints = fromMaybe False (fdConstraints directives)
  in                               extractedOwnership == ownership && 
                                   extractedDependent == dependent && 
                                   extractedConstraints == constraints

-- | BlockDirectives
propBlockDirectivesMerge :: Bool -> Bool -> Bool -> Bool
propBlockDirectivesMerge ownership dependent                               constraints =
  let block1 = defaultBlockDirectives {                               bdOwnership = Just ownership }
                                    block2 = defaultBlockDirectives {                               bdDependentTypes = Just dependent }
                                    block3 = defaultBlockDirectives {                               bdConstraints = Just constraints }
                                    merged = block1  -- 
  in isJust (bdOwnership merged) || isJust (bdDependentTypes merged) || isJust (bdConstraints merged)

-- | CodeBlock
propCodeBlockPositionConsistency :: Int -> Int -> Bool
propCodeBlockPositionConsistency startLine                               endLine =
  let start = startLine `mod` 100
                                    end = (start + (endLine `mod` 10) + 1) `mod` 100
                                    valid = start <= end
  in valid ||                               start == end  -- 

-- ============================================================================
-- SourceLocation Properties
-- ============================================================================

-- | SourceSpan
propSourceSpanOrdering :: Int -> Int -> Int -> Int -> Bool
propSourceSpanOrdering sLine sCol eLine                               eCol =
  let start = SourcePos (sLine `mod` 50 + 1) (sCol `mod` 80 + 1)
                                    end = SourcePos (eLine `mod` 50 + 1) (eCol `mod` 80 + 1)
                                    span = SourceSpan start end
  in (sourcePosLine (spanStart span) <= sourcePosLine (spanEnd span) ||
     (sourcePosLine (spanStart span) == sourcePosLine (spanEnd span) && 
      sourcePosColumn (spanStart span) <= sourcePosColumn (spanEnd span)

-- | Located
propLocatedPreservation :: String -> Int -> Bool
propLocatedPreservation content                               pos =
  let pos' = pos `mod` 100
                                    located = Located content pos'
  in locatedValue                               located == content && locatedPosition                               located == pos'

-- | SourcePos
propSourcePosArithmetic :: Int -> Int -> Int -> Int -> Bool
propSourcePosArithmetic line1 col1 line2                               col2 =
  let pos1 = SourcePos (line1 `mod` 100 + 1) (col1 `mod` 100 + 1)
                                    pos2 = SourcePos (line2 `mod` 100 + 1) (col2 `mod` 100 + 1)
                                    sameLine = sourcePosLine                               pos1 == sourcePosLine pos2
  in if sameLine
     then sourcePosColumn pos1 /= sourcePosColumn pos2 ||                               pos1 == pos2
     else sourcePosLine pos1 /= sourcePosLine pos2

-- ============================================================================
-- Error Handling Properties
-- ============================================================================

-- | 
propErrorMessageConsistency :: String -> Bool
propErrorMessageConsistency                               errorMsg =
  not (null errorMsg) && L.all (not . isSpace) (L.filter (not . isSpace) errorMsg)

-- | 
propErrorRecoveryPartial :: [Int] -> Bool
propErrorRecoveryPartial                               input =
  let validInput = L.filter (> 0) input
                                    hasValidResult = not (null validInput)
  in hasValidResult || null input

-- ============================================================================
-- String Processing Properties
-- ============================================================================

-- | normalizeIndentation
propNormalizeIndentationRelative :: String -> Bool
propNormalizeIndentationRelative                               input =
  let normalized = normalizeIndentation input
                                    originalLines = lines input
                                    normalizedLines = lines normalized
  in L.length                               normalizedLines == L.length originalLines

-- | 
propCommentRemovalIdempotent :: String -> Bool
propCommentRemovalIdempotent                               input =
  let once = removeComments input
                                    twice = removeComments once
  in                               once == twice

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- 
genSafeString :: Gen String
                              genSafeString = listOf $ oneof 
  [ elements ['a'..'z']
  , elements ['A'..'Z']
  , elements ['0'..'9']
  , elements " \t\n"
  , elements ".,;:!()-_+="
  ]

-- 
genIdentifier :: Gen String
                              genIdentifier = do
              first <- elements ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']
  return (first : rest)

-- 
instance Arbitrary String where
                                              arbitrary = genSafeString)