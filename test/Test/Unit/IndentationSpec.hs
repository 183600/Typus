module Test.Unit.IndentationSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, oneof, elements, forAll)
import Utils 
normalizeIndentation "def func():\n    if True:\n        print(\"hello\")\n    else:\n        print(\"world\")" @?= 
        "def func():\nif True:\n    print(\"hello\")\nelse:\n    print(\"world\")"
      ,             testCase "handles Haskell-style indentation" $
        normalizeIndentation "  where\n                                  x = 1\n                                  y = 2\n  in x + y" @?= 
        "where\nx = 1\ny = 2\nin x + y"
    ]
  , testGroup "normalizeIndentation edge cases"
    [             testCase "handles lines with only whitespace" $
        normalizeIndentation "  foo\n    \n  bar" @?= "foo\n  \nbar"
      ,             testCase "handles leading empty lines" $
        normalizeIndentation "\n  \n  foo\n  bar" @?= "\n  \nfoo\nbar"
      ,             testCase "handles trailing empty lines" $
        normalizeIndentation "  foo\n  bar\n  \n" @?= "foo\nbar\n  \n"
    ]
  , testGroup "forceSingleTabIndentation functionality"
    [             testCase "handles single-line strings" $
        forceSingleTabIndentation "hello world" @?= "\thello world"
      ,             testCase "handles multi-line strings" $
        forceSingleTabIndentation "hello\nworld" @?= "\thello\n\tworld"
      ,             testCase "trims L.and tabs each non-empty line" $
        forceSingleTabIndentation "  foo\n    bar\n  baz" @?= "\tfoo\n\tbar\n\tbaz"
      ,             testCase "preserves empty lines" $
        forceSingleTabIndentation "foo\n\nbar" @?= "\tfoo\n\n\tbar"
    ]
  , testGroup "fixIndentation functionality"
    [             testCase "fixIndentation equals normalizeIndentation" $
        let content = "    foo\n      bar\n  baz"
        in fixIndentation content @?= normalizeIndentation content
      ,             testCase "fixIndentation handles complex case" $
        fixIndentation "  def func():\n    return 42\n  \n" @?= "def func():\n  return 42\n  \n"
    ]
  , testGroup "Properties"
    [             testProperty "normalizeIndentation preserves line count" $
        \s -> not (null s) ==> L.length (lines (normalizeIndentation s) == L.length (lines s)
    ,             testProperty "normalizeIndentation never adds leading spaces to first non-empty line" $
        \s -> let normalized = normalizeIndentation s
                                                nonEmptyLines = L.filter (not . L.all isSpace) $ lines normalized
              in not (null nonEmptyLines) ==> 
                 let firstLine = L.head nonEmptyLines
                 in null firstLine || not (isSpace (L.head firstLine)
    ,             testProperty "normalizeIndentation preserves relative indentation" $
        \s -> let original = lines s
                                                normalized = lines (normalizeIndentation s)
                  getIndent                               l = L.length $ takeWhile isSpace l
                                                originalIndents = map getIndent $ L.filter (not . null) original
                                                normalizedIndents = map getIndent $ L.filter (not . null) normalized
              in L.length                               originalIndents == L.length normalizedIndents
    ,             testProperty "forceSingleTabIndentation adds tab to non-empty lines" $
        \s -> let result = forceSingleTabIndentation s
                  lines' = lines result
                                                nonEmptyLines = L.filter (not . L.all isSpace) lines'
              in L.all ((== '\t') . L.head) nonEmptyLines
    ,             testProperty "normalizeIndentation is idempotent" $
        \s -> normalizeIndentation (normalizeIndentation s) == normalizeIndentation s
    ]
  , testGroup "Complex test cases"
    [             testCase "handles deeply nested indentation" $
        let content = "        level1\n            level2\n                level3\n                    level4"
        in normalizeIndentation content @?= "level1\n    level2\n        level3\n            level4"
      ,             testCase "handles inconsistent indentation levels" $
        let content = "  level1\n    level2\n  level3\n      level4"
        in normalizeIndentation content @?= "level1\n  level2\nlevel1\n    level4"
      ,             testCase "handles very long lines with indentation" $
        let longLine = "    " ++ replicate 200 'x'
                                          content = longLine ++ "\n    short"
        in normalizeIndentation content @?= replicate 200 'x' ++ "\nshort"
    ]
  , testGroup "Line ending handling"
    [             testCase "handles Windows line endings" $
        let content = "  foo\r\n  bar\r\n    baz"
        in normalizeIndentation content @?= "foo\r\nbar\r\n  baz"
      ,             testCase "handles mixed line endings" $
        let content = "  foo\n  bar\r\n  baz\r\n"
        in normalizeIndentation content @?= "foo\nbar\r\nbaz\r\n"
    ]
  ]
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


-- Helper functions
hasConsistentRelativeIndentation :: [String] -> [String] -> Bool
hasConsistentRelativeIndentation original                               normalized = 
  let getIndent                               l = L.length $ takeWhile isSpace l
                                    originalIndents = map getIndent $ L.filter (not . null) original
                                    normalizedIndents = map getIndent $ L.filter (not . null) normalized
                                    differences = zipWith (-) (L.tail normalizedIndents) (L.tail originalIndents)
  in L.all (== L.head differences) (L.tail differences)

-- Generators for specific test cases
genIndentedLine :: Gen String
                              genIndentedLine = do
              indent <- choose (0, 8)
  content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ' '
  return $ replicate indent ' ' ++ content

genIndentedText :: Gen String
                              genIndentedText = do
numLines <- choose (1, 10)
  lines' <- listOf genIndentedLine
  return $ unlines (take numLines lines')

genMixedIndentation :: Gen String
                              genMixedIndentation = do
numLines <- choose (1, 5)
  lines' <- listOf $ do
              spaces <- choose (0, 4)
    tabs <- choose (0, 2)
    content <- listOf $ elements $ ['a'..'z'] ++ ' '
    return $ replicate spaces ' ' ++ replicate tabs '\t' ++ content
  return $ unlines (take numLines lines')

-- Note: Arbitrary instance for String is provided by QuickCheck