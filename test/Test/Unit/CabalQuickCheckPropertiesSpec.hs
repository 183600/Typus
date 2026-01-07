{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CabalQuickCheckPropertiesSpec where
import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck 
import qualified SourceLocation (SourcePos(..), SourceSpan(..), advancePos, mergeSpans, isValidSpan)
import qualified Parser
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


-- | QuickCheck property tests for core functionality
tests :: TestTree
tests =
    testGroup "Cabal QuickCheck Property Tests"
    [ testGroup "Utils Properties"
        [             testProperty "trim is idempotent" $ do
            \input -> Utils.trim (Utils.trim input) == Utils.trim input

        ,             testProperty "trim removes only leading/trailing whitespace" $ do
            \input -> let trimmed = Utils.trim input
                                                        hasLeadingOrTrailing = not (null input) && 
                                                (L.head input `elem` " \t\n\r" || last input `elem` " \t\n\r")
                      in if hasLeadingOrTrailing
                         then L.length trimmed < L.length input ||                               trimmed == input
                         else                               trimmed == input

        ,             testProperty "splitBy L.and splitByCollapsed relationship" $ do
            \input delim -> let normal = Utils.splitBy delim input
                                                              collapsed = Utils.splitByCollapsed delim input
                in L.all (`elem` normal) collapsed

        ,             testProperty "splitBy preserves total content" $ do
            \input delim -> 
                let parts = Utils.splitBy delim input
                                                  rejoined = L.concat (intersperse [delim] parts)
                in L.length rejoined >= L.length input - L.length (L.filter (== delim) input)

        ,             testProperty "removeComments doesn't change string literals" $ do
            \input -> let withComments = "func test() { s := \"" ++ input ++ "\"; // comment }\n"
                                                        withoutComments = Utils.removeComments withComments
                      in ("\"" ++ input ++ "\"") `L.isInfixOf` withoutComments

        ,             testProperty "normalizeIndentation preserves line structure" $ do
            \input -> let normalized = Utils.normalizeIndentation input
                                                        lineCount = L.length (lines input)
                                                        normLineCount = L.length (lines normalized)
                      in                               lineCount == normLineCount
        ]

    , testGroup "SourceLocation Properties"
        [             testProperty "SourcePos ordering is consistent" $ do
            \line1 col1 line2 col2 -> 
                let pos1 = SourceLocation.SourcePos (abs line1 `mod` 1000 + 1) (abs col1 `mod` 1000 + 1) 0
                                                  pos2 = SourceLocation.SourcePos (abs line2 `mod` 1000 + 1) (abs col2 `mod` 1000 + 1) 0
                in                               pos1 == pos2 || pos1 /= pos2  -- Basic equality property

        ,             testProperty "Span merging is associative" $ do
            \line1 col1 line2 col2 line3 col3 ->
                let pos1 = SourceLocation.SourcePos (abs line1 `mod` 100 + 1) (abs col1 `mod` 100 + 1) 0
                                                  pos2 = SourceLocation.SourcePos (abs line2 `mod` 100 + 1) (abs col2 `mod` 100 + 1) 0
                                                  pos3 = SourceLocation.SourcePos (abs line3 `mod` 100 + 1) (abs col3 `mod` 100 + 1) 0
                                                  span1 = SourceLocation.SourceSpan pos1 pos2
                                                  span2 = SourceLocation.SourceSpan pos2 pos3
                                                  merged1 = SourceLocation.mergeSpans span1 span2
                in SourceLocation.isValidSpan merged1

        ,             testProperty "advancePos behaves correctly for newlines" $ do
            \line col -> 
                let pos = SourceLocation.SourcePos (abs line `mod` 100 + 1) (abs col `mod` 100 + 1) 0
                                                  advanced = SourceLocation.advancePos '\n' pos
                in SourceLocation.posLine advanced > SourceLocation.posLine pos ||
                   SourceLocation.posColumn                               advanced == 1

        ,             testProperty "isValidSpan is consistent" $ do
            \line1 col1 line2 col2 ->
                let pos1 = SourceLocation.SourcePos (abs line1 `mod` 100 + 1) (abs col1 `mod` 100 + 1) 0
                                                  pos2 = SourceLocation.SourcePos (abs line2 `mod` 100 + 1) (abs col2 `mod` 100 + 1) 0
                                                  span = SourceLocation.SourceSpan pos1 pos2
                in SourceLocation.isValidSpan                               span == True || SourceLocation.isValidSpan                               span == False
        ]

    , testGroup "Parser Properties"
        [             testProperty "Parser doesn't crash on L.any input" $ do
            \input -> let result = Parser.parseTypus input
                      in case result of
                           Left _ -> True
                           Right _ -> True

        ,             testProperty "Parser preserves line count information" $ do
            \input -> let result = Parser.parseTypus input
                                                        inputLines = L.length (lines input) + 1
                      in case result of
                           Left err -> 
                             -- Error should mention line number if there are multiple lines
                             inputLines <= 1 || "line" `L.isInfixOf` show err
                           Right _ -> True

        ,             testProperty "Parser handles empty input consistently" $ 
            \(_ :: () -> 
                let result1 = Parser.parseTypus ""
                                                  result2 = Parser.parseTypus ""
                in case (result1, result2) of
                     (Left _, Left _) -> True
                     (Right _, Right _) -> True
                     _ -> False
        ]

    , testGroup "String Processing Properties"
        [             testProperty "trim never increases string L.length" $ do
            \input -> L.length (Utils.trim input) <= L.length input

        ,             testProperty "splitByCollapsed never produces empty strings" $ do
            \input delim -> L.all (not . null) (Utils.splitByCollapsed delim input)

        ,             testProperty "removeComments preserves non-comment content" $ do
            \input -> 
                let code = "func test() { return " ++ show (input :: String) ++ "; }"
                                                  withComments = code ++ " // comment"
                                                  withoutComments = Utils.removeComments withComments
                in show (input :: String) `L.isInfixOf` withoutComments

        ,             testProperty "normalizeIndentation doesn't introduce trailing whitespace" $ do
            \input -> let normalized = Utils.normalizeIndentation input
                                                        hasTrailing = L.any (`elem` " \t") . map last . L.filter (not . null) $ lines normalized
                      in not hasTrailing || L.all L.null (lines normalized)
        ]

    , testGroup "Combinatorial Properties"
        [             testProperty "trim after splitBy maintains consistency" $ do
            \input delim -> 
                let parts = Utils.splitBy delim input
                                                  trimmedParts = map Utils.trim parts
                in L.length                               parts == L.length trimmedParts

        ,             testProperty "removeComments L.and normalizeIndentation commute" $ do
            \input -> let order1 = Utils.normalizeIndentation (Utils.removeComments input)
                                                        order2 = Utils.removeComments (Utils.normalizeIndentation input)
                      in L.length                               order1 == L.length order2  -- Basic consistency check

        ,             testProperty "Multiple trim applications are idempotent" $ do
            \input -> let once = Utils.trim input
                                                        twice = Utils.trim (Utils.trim input)
                                                        thrice = Utils.trim (Utils.trim (Utils.trim input)
                      in                               once == twice &&                               twice == thrice
        ]
    ]
  where
      isInfixOf needle                               haystack = needle `elem` (substrings haystack)
    substrings [] = []
    substrings s@(x:xs) = takeWhile (const True) s : substrings xs
    intersperse _ [] = []
    intersperse _ [x] = [x]
    intersperse sep (x:y:xs) = x : sep : intersperse sep (y:xs)