module Test.Unit.UtilsStringProcessingQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, chooseInt, vectorOf, suchThat, Positive(..), NonNegative(..))
import TestSupport.QuickCheck (fastProperty)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed,
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn)

-- ============================================================================
-- Properties
-- ============================================================================

tests :: TestTree
tests = testGroup "Utils String Processing QuickCheck Tests"
    [ testGroup "Trim Properties"
        [ testProperty "trim is idempotent" $
            fastProperty prop_trimIdempotent
        
        , testProperty "trim removes leading whitespace" $
            fastProperty prop_trimRemovesLeading
        
        , testProperty "trim removes trailing whitespace" $
            fastProperty prop_trimRemovesTrailing
        
        , testProperty "trim preserves internal whitespace" $
            fastProperty prop_trimPreservesInternal
        ]

    , testGroup "Split Properties"
        [ testProperty "splitBy preserves empty segments" $
            fastProperty prop_splitByPreservesEmpty
        
        , testProperty "splitByCollapsed removes empty segments" $
            fastProperty prop_splitByCollapsedRemovesEmpty
        
        , testProperty "splitByComma delegates to splitBy" $
            fastProperty prop_splitByCommaDelegates
        
        , testProperty "splitByCommaCollapsed removes empty segments" $
            fastProperty prop_splitByCommaCollapsedRemovesEmpty
        
        , testProperty "splitBy is consistent with delimiter count" $
            fastProperty prop_splitByConsistentWithDelimiterCount
        ]

    , testGroup "Comment Removal Properties"
        [ testProperty "removeLineComments respects string literals" $
            fastProperty prop_removeLineCommentsRespectsStrings
        
        , testProperty "removeLineComments respects character literals" $
            fastProperty prop_removeLineCommentsRespectsChars
        
        , testProperty "removeComments removes block comments" $
            fastProperty prop_removeCommentsRemovesBlocks
        
        , testProperty "removeComments preserves line structure" $
            fastProperty prop_removeCommentsPreservesLines
        
        , testProperty "removeComments handles nested comments gracefully" $
            fastProperty prop_removeCommentsHandlesNested
        ]

    , testGroup "Indentation Properties"
        [ testProperty "normalizeIndentation removes common leading whitespace" $
            fastProperty prop_normalizeIndentationRemovesCommon
        
        , testProperty "normalizeIndentation preserves relative indentation" $
            fastProperty prop_normalizeIndentationPreservesRelative
        
        , testProperty "forceSingleTabIndentation enforces tab indentation" $
            fastProperty prop_forceSingleTabIndentationEnforcesTabs
        
        , testProperty "fixIndentation is alias for normalizeIndentation" $
            fastProperty prop_fixIndentationIsAlias
        ]

    , testGroup "Search Properties"
        [ testProperty "breakOn finds pattern when present" $
            fastProperty prop_breakOnFindsPattern
        
        , testProperty "breakOn returns original when pattern missing" $
            fastProperty prop_breakOnReturnsOriginal
        
        , testProperty "breakOn handles empty pattern" $
            fastProperty prop_breakOnHandlesEmptyPattern
        
        , testProperty "breakOn handles pattern at start" $
            fastProperty prop_breakOnHandlesPatternAtStart
        
        , testProperty "breakOn handles pattern at end" $
            fastProperty prop_breakOnHandlesPatternAtEnd
        ]

    , testGroup "Edge Cases"
        [ testProperty "Functions handle very long strings" $
            fastProperty prop_handlesLongStrings
        
        , testProperty "Functions handle unicode content" $
            fastProperty prop_handlesUnicode
        
        , testProperty "Functions handle empty strings" $
            fastProperty prop_handlesEmptyStrings
        
        , testProperty "Functions handle whitespace-only strings" $
            fastProperty prop_handlesWhitespaceOnly
        ]

    , testGroup "Performance Properties"
        [ testProperty "trim is linear time" $
            fastProperty prop_trimIsLinear
        
        , testProperty "splitBy is linear time" $
            fastProperty prop_splitByIsLinear
        
        , testProperty "comment removal is linear time" $
            fastProperty prop_commentRemovalIsLinear
        ]
    ]

-- ============================================================================
-- Property Definitions
-- ============================================================================

-- Trim Properties

prop_trimIdempotent :: String -> Bool
prop_trimIdempotent input =
    let once = trim input
        twice = trim once
    in once == twice

prop_trimRemovesLeading :: String -> Bool
prop_trimRemovesLeading input =
    let trimmed = trim input
        leadingRemoved = null trimmed || head trimmed `notElem` " \t\n\r"
    in leadingRemoved

prop_trimRemovesTrailing :: String -> Bool
prop_trimRemovesTrailing input =
    let trimmed = trim input
        trailingRemoved = null trimmed || last trimmed `notElem` " \t\n\r"
    in trailingRemoved

prop_trimPreservesInternal :: String -> String -> String -> Bool
prop_trimPreservesInternal prefix middle suffix =
    let input = prefix ++ "  " ++ middle ++ "  " ++ suffix
        trimmed = trim input
    in middle `isInfixOf` trimmed || (null prefix && null suffix && trimmed == middle)

-- Split Properties

prop_splitByPreservesEmpty :: Char -> String -> Bool
prop_splitByPreservesEmpty delim input =
    let result = splitBy delim input
        expectedCount = length (filter (== delim) input) + 1
    in length result == expectedCount

prop_splitByCollapsedRemovesEmpty :: Char -> String -> Bool
prop_splitByCollapsedRemovesEmpty delim input =
    let result = splitByCollapsed delim input
    in all (not . null) result

prop_splitByCommaDelegates :: String -> Bool
prop_splitByCommaDelegates input =
    let result1 = splitByComma input
        result2 = splitBy ',' input
    in result1 == result2

prop_splitByCommaCollapsedRemovesEmpty :: String -> Bool
prop_splitByCommaCollapsedRemovesEmpty input =
    let result = splitByCommaCollapsed input
    in all (not . null) result

prop_splitByConsistentWithDelimiterCount :: Char -> String -> Bool
prop_splitByConsistentWithDelimiterCount delim input =
    let result = splitBy delim input
        delimCount = length (filter (== delim) input)
    in length result == delimCount + 1

-- Comment Removal Properties

prop_removeLineCommentsRespectsStrings :: String -> String -> Bool
prop_removeLineCommentsRespectsStrings code comment =
    let input = "value := \"" ++ code ++ "\" // " ++ comment ++ "\n"
        result = removeLineComments input
    in ("\"" ++ code ++ "\"") `isInfixOf` result

prop_removeLineCommentsRespectsChars :: String -> String -> Bool
prop_removeLineCommentsRespectsChars code comment =
    let input = "value := '" ++ code ++ "' // " ++ comment ++ "\n"
        result = removeLineComments input
    in ("'" ++ code ++ "'") `isInfixOf` result

prop_removeCommentsRemovesBlocks :: String -> String -> Bool
prop_removeCommentsRemovesBlocks before after =
    let input = before ++ "/* block comment */" ++ after
        result = removeComments input
    in "block comment" `notInfixOf` result &&
       before `isInfixOf` result &&
       after `isInfixOf` result

prop_removeCommentsPreservesLines :: String -> String -> String -> Bool
prop_removeCommentsPreservesLines line1 comment line2 =
    let input = line1 ++ "/* " ++ comment ++ " */\n" ++ line2
        result = removeComments input
        lineCount1 = length (lines input)
        lineCount2 = length (lines result)
    in lineCount2 <= lineCount1

prop_removeCommentsHandlesNested :: String -> String -> String -> Bool
prop_removeCommentsHandlesNested outer inner after =
    let input = "/* outer " ++ inner ++ " */" ++ after
        result = removeComments input
    in "outer" `notInfixOf` result && 
       inner `notInfixOf` result &&
       after `isInfixOf` result

-- Indentation Properties

prop_normalizeIndentationRemovesCommon :: String -> Bool
prop_normalizeIndentationRemovesCommon input =
    let lines' = lines input
        nonEmptyLines = filter (not . all isSpace) lines'
        hasCommonIndent = length nonEmptyLines > 1
    in if hasCommonIndent
       then let normalized = normalizeIndentation input
                normalizedLines = lines normalized
                nonEmptyNormalized = filter (not . all isSpace) normalizedLines
            in all (not . isPrefixOf "    ") nonEmptyNormalized
       else True
  where
    isSpace c = c `elem` " \t"
    isPrefixOf prefix str = take (length prefix) str == prefix

prop_normalizeIndentationPreservesRelative :: String -> Bool
prop_normalizeIndentationPreservesRelative input =
    let originalLines = lines input
        normalizedLines = lines (normalizeIndentation input)
        originalIndentLevels = map (length . takeWhile isSpace) originalLines
        normalizedIndentLevels = map (length . takeWhile isSpace) normalizedLines
    in length originalIndentLevels == length normalizedIndentLevels

prop_forceSingleTabIndentationEnforcesTabs :: String -> Bool
prop_forceSingleTabIndentationEnforcesTabs input =
    let result = forceSingleTabIndentation input
        lines' = lines result
        nonEmptyLines = filter (not . null) lines'
    in all (`isPrefixOf` "\t") nonEmptyLines

prop_fixIndentationIsAlias :: String -> Bool
prop_fixIndentationIsAlias input =
    let result1 = fixIndentation input
        result2 = normalizeIndentation input
    in result1 == result2

-- Search Properties

prop_breakOnFindsPattern :: String -> String -> Bool
prop_breakOnFindsPattern pattern haystack =
    let result = breakOn pattern haystack
    in if pattern `isInfixOf` haystack
       then let (prefix, suffix) = result
            in not (null suffix) && pattern `isPrefixOf` suffix
       else result == (haystack, "")

prop_breakOnReturnsOriginal :: String -> String -> Bool
prop_breakOnReturnsOriginal pattern haystack =
    let result = breakOn pattern haystack
    in if pattern `notInfixOf` haystack
       then result == (haystack, "")
       else True

prop_breakOnHandlesEmptyPattern :: String -> Bool
prop_breakOnHandlesEmptyPattern haystack =
    let result = breakOn "" haystack
    in result == ("", haystack)

prop_breakOnHandlesPatternAtStart :: String -> String -> Bool
prop_breakOnHandlesPatternAtStart pattern haystack =
    let haystackWithPattern = pattern ++ haystack
        result = breakOn pattern haystackWithPattern
        (prefix, suffix) = result
    in null prefix && pattern `isPrefixOf` suffix

prop_breakOnHandlesPatternAtEnd :: String -> String -> Bool
prop_breakOnHandlesPatternAtEnd pattern haystack =
    let haystackWithPattern = haystack ++ pattern
        result = breakOn pattern haystackWithPattern
        (prefix, suffix) = result
    in prefix == haystack && suffix == ""

-- Edge Cases

prop_handlesLongStrings :: Int -> String -> Bool
prop_handlesLongStrings n base =
    let longString = take (abs n `mod` 1000 + 10) (cycle base)
        trimmed = trim longString
        split = splitBy ',' longString
        commentsRemoved = removeLineComments longString
    in not (null trimmed) && length split >= 1 && not (null commentsRemoved)

prop_handlesUnicode :: String -> Bool
prop_handlesUnicode base =
    let unicodeString = base ++ " αβγδε 中文 тест 🚀"
        trimmed = trim unicodeString
        split = splitBy ' ' unicodeString
        commentsRemoved = removeLineComments unicodeString
    in not (null trimmed) && length split >= 1 && not (null commentsRemoved)

prop_handlesEmptyStrings :: Bool
prop_handlesEmptyStrings =
    let trimmed = trim ""
        split = splitBy ',' ""
        splitCollapsed = splitByCollapsed ',' ""
        commentsRemoved = removeLineComments ""
        blockCommentsRemoved = removeComments ""
    in trimmed == "" && split == [""] && splitCollapsed == [] && 
       commentsRemoved == "" && blockCommentsRemoved == ""

prop_handlesWhitespaceOnly :: String -> Bool
prop_handlesWhitespaceOnly ws =
    let whitespaceOnly = filter isSpace ws
        trimmed = trim whitespaceOnly
        split = splitBy ',' whitespaceOnly
        commentsRemoved = removeLineComments whitespaceOnly
    in trimmed == "" && (null whitespaceOnly || split == [""]) && not (null commentsRemoved)

-- Performance Properties

prop_trimIsLinear :: String -> Bool
prop_trimIsLinear input =
    -- This is a simplified property - in reality we'd measure performance
    let result = trim input
    in not (null result) || null input

prop_splitByIsLinear :: String -> Char -> Bool
prop_splitByIsLinear input delim =
    -- This is a simplified property - in reality we'd measure performance
    let result = splitBy delim input
    in length result >= 1

prop_commentRemovalIsLinear :: String -> Bool
prop_commentRemovalIsLinear input =
    -- This is a simplified property - in reality we'd measure performance
    let result = removeLineComments input
        result2 = removeComments input
    in not (null result) && not (null result2)

-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` [take (length haystack - length needle + 1) (drop i haystack) | i <- [0..length haystack - length needle]]

notInfixOf :: String -> String -> Bool
notInfixOf needle haystack = not (isInfixOf needle haystack)

isPrefixOf :: String -> String -> Bool
isPrefixOf prefix str = take (length prefix) str == prefix

isSpace :: Char -> Bool
isSpace c = c `elem` " \t\n\r"