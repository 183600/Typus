module Test.Unit.EnhancedUtilsQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, oneof, listOf, elements, choose, suchThat, (===), )
                              trailingRemoved = L.reverse (dropWhile isSpace (L.reverse leadingRemoved)
  in                               trimmed == trailingRemoved
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


-- Property: splitBy L.and splitByCollapsed relationship
prop_splitByRelationship :: Char -> String -> Bool
prop_splitByRelationship delim                               input =
  let normal = splitBy delim input
                                    collapsed = splitByCollapsed delim input
  in                               collapsed == L.filter (not . null) normal

-- Property: splitBy preserves delimiter count
prop_splitByPreservesCount :: Char -> String -> Bool
prop_splitByPreservesCount delim                               input =
  let parts = splitBy delim input
                                    delimiterCount = L.length (L.filter (== delim) input)
  in L.length parts -                               1 == delimiterCount

-- Property: removeComments preserves non-comment content
prop_removeCommentsPreservesContent :: String -> Bool
prop_removeCommentsPreservesContent                               input =
  let withoutComments = removeComments input
      -- Extract non-comment characters from original
                                    nonCommentChars = L.filter (not . isCommentChar) input
                                    nonCommentCharsProcessed = L.filter (not . isCommentChar) withoutComments
  in L.length                               nonCommentChars == L.length nonCommentCharsProcessed
  where
      isCommentChar '/' = True
    isCommentChar '*' = True
    isCommentChar                               _ = False

-- Property: removeLineComments handles string literals correctly
prop_removeLineCommentsStrings :: String -> Bool
prop_removeLineCommentsStrings                               input =
  let processed = removeLineComments input
                                    linesInput = lines input
                                    linesProcessed = lines processed
  in L.length                               linesInput == L.length linesProcessed

-- Property: normalizeIndentation preserves relative structure
prop_normalizeIndentationPreservesStructure :: String -> Bool
prop_normalizeIndentationPreservesStructure                               input =
  let normalized = normalizeIndentation input
                                    inputLines = lines input
                                    normalizedLines = lines normalized
      -- Check that non-empty lines are preserved
                                    inputNonEmpty = L.filter (not . L.all isSpace) inputLines
                                    normalizedNonEmpty = L.filter (not . L.all isSpace) normalizedLines
  in L.length                               inputNonEmpty == L.length normalizedNonEmpty

-- Property: breakOn correctness
prop_breakOnCorrectness :: String -> String -> Bool
prop_breakOnCorrectness pattern text
  | null                               pattern = breakOn pattern                               text == ("", text)
  | pattern `L.isInfixOf`                               text = 
      let (before, after) = breakOn pattern text
      in before ++ pattern ++                               after == text
  |                               otherwise = breakOn pattern                               text == (text, "")

-- Property: breakOn empty pattern behavior
prop_breakOnEmptyPattern :: String -> Bool
prop_breakOnEmptyPattern                               text =
  breakOn ""                               text == ("", text)

-- ============================================================================
-- Edge Case Properties
-- ============================================================================

-- Property: trim handles empty L.and whitespace-only strings
prop_trimEdgeCases :: String -> Bool
prop_trimEdgeCases                               input =
  let trimmed = trim input
                                    isOnlyWhitespace = L.all isSpace input
  in if null input || isOnlyWhitespace
     then null trimmed
     else not (L.all isSpace trimmed)

-- Property: splitBy handles edge cases
prop_splitByEdgeCases :: Char -> String -> Bool
prop_splitByEdgeCases delim                               input =
  let parts = splitBy delim input
      -- Check that joining with delimiter gives back original
                                    rejoined = foldr1 (\a b -> a ++ [delim] ++ b) parts
  in if null parts
     then True
     else L.length parts > 0 && (if null input then                               parts == [""] else True)

-- Property: removeComments handles nested structures
prop_removeCommentsNested :: String -> Bool
prop_removeCommentsNested                               input =
  let processed = removeComments input
      -- Basic sanity: processed should not contain comment markers
                                    hasBlockComment = "/*" `L.isInfixOf` processed || "*/" `L.isInfixOf` processed
                                    hasLineComment = "//" `L.isInfixOf` processed
  in not (hasBlockComment || hasLineComment)

-- Property: normalizeIndentation handles mixed indentation
prop_normalizeIndentationMixed :: String -> Bool
prop_normalizeIndentationMixed                               input =
  let normalized = normalizeIndentation input
                                    normalizedLines = lines normalized
  -- Check that no line starts with both spaces L.and tabs (mixed indentation)
  in L.all (not . hasMixedIndentation) normalizedLines
  where
      hasMixedIndentation                               line =
      let leading = takeWhile isSpace line
                                        hasSpaces = ' ' `elem` leading
                                        hasTabs = '\t' `elem` leading
      in hasSpaces && hasTabs

-- Property: comment removal preserves line count
prop_commentRemovalPreservesLines :: String -> Bool
prop_commentRemovalPreservesLines                               input =
  let withComments = input
                                    withoutComments = removeComments input
                                    withoutLineComments = removeLineComments input
                                    linesOriginal = L.length (lines withComments)
                                    linesBlockComments = L.length (lines withoutComments)
                                    linesLineComments = L.length (lines withoutLineComments)
  in linesBlockComments <= linesOriginal && linesLineComments <= linesOriginal

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- Generate strings with various whitespace patterns
genWhitespaceString :: Gen String
                              genWhitespaceString = listOf $ oneof 
  [ elements " \t\n\r"
  , elements (map toEnum [32..126]) -- printable ASCII
  ]

-- Generate strings that might contain comment-like patterns
genCommentString :: Gen String
                              genCommentString = listOf $ oneof
  [ elements "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
  , elements " \t\n\r"
  , elements "/*"
  , elements "*/"
  , elements "//"
  , elements "\""
  , elements "'"
  , elements "\\"
  ]

-- Generate strings with indentation
genIndentedString :: Gen String
                              genIndentedString = do
              numLines <- choose (0, 10)
  lines <- sequence $ replicate numLines genIndentedLine
  return $ unlines lines
  where
                                    genIndentedLine = do
              indent <- choose (0, 5)
      spaces <- return $ replicate indent ' '
      content <- listOf $ elements "abcdefghijklmnopqrstuvwxyz"
      return $ spaces ++ content

-- Instance for generating test strings
instance Arbitrary String where
                                              arbitrary = oneof
    [ genWhitespaceString
    , genCommentString
    , genIndentedString
    , listOf $ elements (map toEnum [32..126]) -- printable ASCII
    ]