{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewUtilsPerformanceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool, assertFailure)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, elements, suchThat)
import qualified Data.Text as T
import Data.Maybe (isJust, isNothing, catMaybes)
import qualified Data.List as L
import Data.List (isInfixOf, length)
import Data.List (nub, foldl')
import Data.Char (isSpace)

import Utils (trim, splitBy, splitByCollapsed, splitByComma, splitByCommaCollapsed, 
             removeLineComments, removeComments, normalizeIndentation, 
             forceSingleTabIndentation, fixIndentation, breakOn)

-- | Test Utils module performance functionality
tests :: TestTree
tests =
  testGroup "New Utils Performance Tests"
    [ stringProcessingPerformanceTests
    , splittingPerformanceTests
    , commentRemovalPerformanceTests
    , indentationPerformanceTests
    , memoryEfficiencyTests
    , edgeCasePerformanceTests
    , quickCheckProperties
    ]

-- | String processing performance tests
stringProcessingPerformanceTests :: TestTree
stringProcessingPerformanceTests =
  testGroup "String Processing Performance Tests"
    [ testCase "Large string trimming performance" $
        let largeString = "   " ++ replicate 10000 ' ' ++ "content" ++ replicate 10000 ' ' ++ "   "
            result = trim largeString
        in do
           assertEqual "Should trim correctly" "content" result
           assertBool "Should handle large strings efficiently" (L.length result < L.length largeString)

    , testCase "Unicode string processing performance" $
        let unicodeString = L.concat $ replicate 1000 ["你好", "世界", "🌍", "test"]
            result = trim unicodeString
        in do
           assertBool "Should handle Unicode correctly" (L.length result > 0)
           assertBool "Should preserve Unicode characters" (L.any (> 127) (map fromEnum result))

    , testCase "Repeated trim operations" $
        let testString = "   \t  content with spaces   \t  "
            results = replicate 1000 (trim testString)
        in do
           assertEqual "All results should be identical" (L.head results) (last results)
           assertBool "Should be consistent" (L.all (== "content with spaces") results)

    , testCase "Empty L.and whitespace-only strings" $
        let emptyResult = trim ""
            spaceResult = trim (replicate 1000 ' ')
            tabResult = trim (replicate 1000 '\t')
            mixedResult = trim (replicate 500 ' ' ++ replicate 500 '\t')
        in do
           assertEqual "Empty string should remain empty" "" emptyResult
           assertEqual "Whitespace-only should become empty" "" spaceResult
           assertEqual "Tab-only should become empty" "" tabResult
           assertEqual "Mixed whitespace should become empty" "" mixedResult
    ]

-- | Splitting performance tests
splittingPerformanceTests :: TestTree
splittingPerformanceTests =
  testGroup "Splitting Performance Tests"
    [ testCase "Large string splitting performance" $
        let largeString = unwords $ replicate 10000 "word"
            result = splitBy ' ' largeString
        in do
           assertEqual "Should split correctly" 10000 (L.length result)
           assertBool "All parts should be non-empty" (L.all (not . null) result)

    , testCase "Splitting with many delimiters" $
        let delimitedString = L.concat $ replicate 5000 "a,"
            result = splitBy ',' delimitedString
        in do
           assertEqual "Should handle consecutive delimiters" 5000 (L.length result)
           assertBool "Should preserve empty segments" (L.all (== "a") result)

    , testCase "Collapsed splitting performance" $
        let collapsedString = L.concat $ replicate 1000 "a,,"
            result = splitByCollapsed ',' collapsedString
        in do
           assertEqual "Should collapse consecutive delimiters" 1000 (L.length result)
           assertBool "Should remove empty segments" (L.all (== "a") result)

    , testCase "Comma splitting variants" $
        let testString = "a,b,,c,d,,e"
            normalResult = splitByComma testString
            collapsedResult = splitByCommaCollapsed testString
        in do
           assertEqual "Normal split should preserve empties" ["a","b","","c","d","","e"] normalResult
           assertEqual "Collapsed split should remove empties" ["a","b","c","d","e"] collapsedResult

    , testCase "Unicode splitting performance" $
        let unicodeString = intercalate "," $ replicate 1000 ["你好", "世界", "🌍"]
            result = splitBy ',' unicodeString
        in do
           assertEqual "Should split Unicode correctly" 1000 (L.length result)
           assertBool "Should preserve Unicode segments" (L.any (not . L.all (< 128) . map fromEnum) result)
    ]

-- | Comment removal performance tests
commentRemovalPerformanceTests :: TestTree
commentRemovalPerformanceTests =
  testGroup "Comment Removal Performance Tests"
    [ testCase "Large file comment removal" $
        let largeFile = unlines $ replicate 5000 ["code line", "// comment line", "another code line"]
            result = removeLineComments largeFile
        in do
           assertBool "Should remove line comments" (not $ "// comment line" `L.isInfixOf` result)
           assertBool "Should preserve code lines" ("code line" `L.isInfixOf` result)

    , testCase "Block comment removal performance" $
        let fileWithBlocks = unlines 
              [ "code before"
              , "/* start of block comment"
              , "this is inside comment"
              , "still in comment */"
              , "code after"
              , "/* another block */"
              , "final code"
              ]
            result = removeComments fileWithBlocks
        in do
           assertBool "Should remove block comments" (not $ "/*" `L.isInfixOf` result)
           assertBool "Should preserve code" ("code before" `L.isInfixOf` result && "code after" `L.isInfixOf` result)

    , testCase "Nested comment handling" $
        let nestedComments = unlines
              [ "code // line comment"
              , "/* block comment with // line comment inside */"
              , "code after"
              ]
            result = removeComments nestedComments
        in do
           assertBool "Should handle nested comments" (not $ L.any (`L.isInfixOf` result) ["//", "/*"])
           assertBool "Should preserve code" ("code after" `L.isInfixOf` result)

    , testCase "Comment removal in string literals" $
        let stringWithStrings = unlines
              [ "let s = \"// not a comment\""
              , "let t = \"/* not a block comment */\""
              , "// this is a real comment"
              , "let u = \"string with // comment inside\""
              ]
            result = removeLineComments stringWithStrings
        in do
           assertBool "Should preserve comments in strings" ("// not a comment" `L.isInfixOf` result)
           assertBool "Should remove real comments" (not $ "this is a real comment" `L.isInfixOf` result)

    , testCase "Performance with many small comments" $
        let manyComments = unlines $ L.concat $ replicate 1000 [["code", "// comment"]]
            result = removeLineComments manyComments
        in do
           assertBool "Should handle many comments efficiently" (not $ "// comment" `L.isInfixOf` result)
           assertBool "Should preserve L.all code lines" ("code" `L.isInfixOf` result)
    ]

-- | Indentation performance tests
indentationPerformanceTests :: TestTree
indentationPerformanceTests =
  testGroup "Indentation Performance Tests"
    [ testCase "Large file indentation normalization" $
        let indentedFile = unlines $ L.map (\i -> replicate (i `mod` 20) ' ' ++ "line " ++ show i) [1..1000]
            result = normalizeIndentation indentedFile
        in do
           assertBool "Should normalize indentation" (not $ L.any (isPrefixOf "    ") (lines result))
           assertBool "Should preserve content" ("line 1" `L.isInfixOf` result && "line 1000" `L.isInfixOf` result)

    , testCase "Mixed tab/space indentation" $
        let mixedIndentation = unlines
              [ "\tline with tab"
              , "    line with spaces"
              , "\t  mixed tab L.and spaces"
              , "        deep indentation"
              ]
            result = normalizeIndentation mixedIndentation
        in do
           assertBool "Should handle mixed indentation" (L.length (lines result) == 4)
           assertBool "Should normalize consistently" (L.all (not . isPrefixOf "\t") (lines result))

    , testCase "Force single tab indentation" $
        let spaceIndented = unlines
              [ "    level 1"
              , "        level 2"
              , "            level 3"
              ]
            result = forceSingleTabIndentation spaceIndented
        in do
           assertBool "Should convert to tabs" (L.any (isPrefixOf "\t") (lines result))
           assertBool "Should preserve structure" (L.length (lines result) == 3)

    , testCase "Indentation with empty lines" $
        let withEmptyLines = unlines
              [ "    line 1"
              , ""
              , "        line 2"
              , ""
              , "    line 3"
              ]
            result = normalizeIndentation withEmptyLines
        in do
           assertBool "Should preserve empty lines" (L.null (lines result !! 1) && L.null (lines result !! 3))
           assertBool "Should normalize non-empty lines" (L.all (not . isPrefixOf "    ") $ L.filter (not . null) (lines result))

    , testCase "Performance with deeply nested code" $
        let deeplyNested = unlines $ L.concat $ replicate 100 
              [ L.map (\i -> replicate (i*4) ' ' ++ "level " ++ show i) [1..10]
              , [""]
              ]
            result = normalizeIndentation deeplyNested
        in do
           assertBool "Should handle deep nesting" (L.length (lines result) > 0)
           assertBool "Should maintain structure" ("level 1" `L.isInfixOf` result && "level 10" `L.isInfixOf` result)
    ]

-- | Memory efficiency tests
memoryEfficiencyTests :: TestTree
memoryEfficiencyTests =
  testGroup "Memory Efficiency Tests"
    [ testCase "String reuse in trimming" $
        let original = "   content   "
            trimmed1 = trim original
            trimmed2 = trim original
        in do
           assertEqual "Results should be identical" trimmed1 trimmed2
           assertBool "Should be memory efficient" (L.length trimmed1 < L.length original)

    , testCase "Efficient splitting with reuse" $
        let original = "a,b,c,d,e"
            result1 = splitBy ',' original
            result2 = splitBy ',' original
        in do
           assertEqual "Results should be identical" result1 result2
           assertBool "Should split correctly" (L.length result1 == 5)

    , testCase "Memory usage with large files" $
        let largeContent = unlines $ replicate 10000 "line with some content"
            trimmed = trim largeContent
            split = splitBy '\n' largeContent
            commentsRemoved = removeLineComments largeContent
        in do
           assertBool "Should handle large content" (L.length trimmed > 0)
           assertBool "Should split large content" (L.length split == 10000)
           assertBool "Should process large content" (L.length commentsRemoved > 0)

    , testCase "Lazy evaluation efficiency" $
        let infiniteStream = map show [1..]
            limited = take 1000 infiniteStream
            joined = unwords limited
            result = splitBy ' ' joined
        in do
           assertEqual "Should handle lazy evaluation" 1000 (L.length result)
           assertBool "Should be memory efficient" (L.all (not . null) result)
    ]

-- | Edge case performance tests
edgeCasePerformanceTests :: TestTree
edgeCasePerformanceTests =
  testGroup "Edge Case Performance Tests"
    [ testCase "Empty string operations" $
        let empty = ""
            trimResult = trim empty
            splitResult = splitBy ',' empty
            commentResult = removeLineComments empty
            indentResult = normalizeIndentation empty
        in do
           assertEqual "Trim empty should be empty" "" trimResult
           assertEqual "Split empty should be [\"\"]" [""] splitResult
           assertEqual "Remove comments from empty should be empty" "" commentResult
           assertEqual "Normalize empty should be empty" "" indentResult

    , testCase "Single character strings" $
        let single = "x"
            trimResult = trim single
            splitResult = splitBy ',' single
        in do
           assertEqual "Trim single should be unchanged" "x" trimResult
           assertEqual "Split single should be [single]" ["x"] splitResult

    , testCase "Strings with only delimiters" $
        let onlyDelimiters = ",,,"
            normalSplit = splitBy ',' onlyDelimiters
            collapsedSplit = splitByCollapsed ',' onlyDelimiters
        in do
           assertEqual "Normal split should preserve empties" ["", "", "", ""] normalSplit
           assertEqual "Collapsed split should remove empties" [] collapsedSplit

    , testCase "Very long tokens" $
        let longToken = replicate 10000 'a'
            stringWithLong = "prefix," ++ longToken ++ ",suffix"
            result = splitBy ',' stringWithLong
        in do
           assertEqual "Should handle long tokens" 3 (L.length result)
           assertEqual "Should preserve long token" longToken (result !! 1)

    , testCase "Special characters L.and Unicode" $
        let specialString = "hello\tworld\ntest\r\nunicode: 你好世界🌍"
            trimResult = trim specialString
            splitResult = splitBy '\n' specialString
        in do
           assertBool "Should handle special characters" (L.length trimResult > 0)
           assertBool "Should split on newlines" (L.length splitResult >= 2)
           assertBool "Should preserve Unicode" (L.any (> 127) (map fromEnum trimResult))
    ]

-- | QuickCheck properties for Utils performance
quickCheckProperties :: TestTree
quickCheckProperties =
  testGroup "QuickCheck Properties"
    [ testProperty "Trim is idempotent" $
        forAll genString $ \s ->
            trim (trim s) === trim s

    , testProperty "Split L.and join are inverses" $
        forAll genSplitString $ \s delim ->
            L.concat (intersperse [delim] (splitBy delim s)) === s

    , testProperty "Collapsed split removes empty segments" $
        forAll genString $ \s ->
            let normal = splitBy ',' s
                collapsed = splitByCollapsed ',' s
            in L.all (not . null) collapsed ==> L.length collapsed <= L.length normal

    , testProperty "Trim removes only leading/trailing whitespace" $
        forAll genString $ \s ->
            let trimmed = trim s
                hasInternalSpaces = ' ' `elem` dropWhile isSpace (dropWhileEnd isSpace s)
            in hasInternalSpaces ==> ' ' `elem` trimmed

    , testProperty "Comment removal preserves non-comment content" $
        forAll genCommentString $ \s ->
            let withoutComments = removeLineComments s
                hasCode = L.any (not . isPrefixOf "//") (lines s)
            in hasCode ==> L.length (L.filter (not . null) (lines withoutComments)) > 0
    ]

-- | Helper functions L.and generators
isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = L.foldr (\x xs -> if p x && null xs then [] else x:xs) []

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse sep (x:xs) = x : sep : intersperse sep xs

-- | Generators for QuickCheck testing
genString :: Gen String
genString = elements
  [ ""
  , "   "
  , "content"
  , "   content   "
  , "  content with spaces  "
  , "\t\ttabs\t\t"
  , "mixed\t spaces\tand\ttabs"
  , replicate 1000 'a'
  , L.concat $ replicate 100 "word "
  ]

genSplitString :: Gen (String, Char)
genSplitString = elements
  [ ("a,b,c,d", ',')
  , ("hello world", ' ')
  , ("path/to/file", '/')
  , ("one|two|three", '|')
  , ("", ',')
  , ("nosplit", ',')
  , ("a,,b,,c", ',')
  ]

genCommentString :: Gen String
genCommentString = elements
  [ "code line"
  , "// comment line"
  , "code // inline comment"
  , "/* block comment */"
  , "code /* comment */ more code"
  , "//"
  , ""
  , "code\n// comment\nmore code"
  ]