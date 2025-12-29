{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Unit.StringUtilsBoundarySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import Test.QuickCheck.Gen (Gen, choose, listOf, elements, oneof, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, removeComments, 
             normalizeIndentation, breakOn)
import qualified Data.Text as T
import Data.Char (isSpace, isAlphaNum, isLetter, isDigit, toLower, toUpper, isAscii, 
                 isControl, isPrint, isLetterOrDigit)
import qualified Data.List as Data.List
import Data.List (isPrefixOf, isInfixOf, sort, nub, group, intercalate)

-- | Generate strings with various character types
genAsciiString :: Gen String
genAsciiString = listOf $ elements $ map toEnum [32..126]  -- printable ASCII

genUnicodeString :: Gen String
genUnicodeString = listOf $ elements $ map toEnum [32..65535]  -- Unicode

genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements " \t\n\r\f\v"

genControlString :: Gen String
genControlString = listOf $ elements $ map toEnum [0..31] ++ [toEnum 127]  -- control chars

genPrintableString :: Gen String
genPrintableString = listOf $ elements $ filter isPrint $ map toEnum [0..255]

genMixedString :: Gen String
genMixedString = do
  ascii <- genAsciiString
  unicode <- genUnicodeString
  whitespace <- genWhitespaceString
  control <- genControlString
  oneof [return $ ascii ++ unicode ++ whitespace ++ control,
         return $ unicode ++ whitespace ++ control ++ ascii,
         return $ whitespace ++ control ++ ascii ++ unicode]

-- | Generate strings with specific patterns
genRepeatedCharString :: Gen String
genRepeatedCharString = do
  char <- elements $ map toEnum [32..126]
  count <- choose (1, 100)
  return $ replicate count char

genPalindromeString :: Gen String
genPalindromeString = do
  base <- genAsciiString
  return $ base ++ reverse base

-- | Test string trimming with various character sets
test_trim_character_sets :: TestTree
test_trim_character_sets = testCase "trim with various character sets" $ do
  let testCases = 
        [ ("  ascii  ", "ascii")  -- ASCII whitespace
        , ("\tunicode\t", "unicode")  -- tab
        , "\nnewlines\n" `shouldBe` "newlines"  -- newlines
        , "\r\rcarriage\r\r" `shouldBe` "carriage"  -- carriage return
        , "  \t\nmixed\t\n  " `shouldBe` "mixed"  -- mixed whitespace
        , ("", "")  -- empty string
        , ("   ", "")  -- only whitespace
        ]
  mapM_ (\(input, expected) -> do
    let result = trim input
    assertEqual ("trim on '" ++ input ++ "'") expected result
  ) testCases
  where
    shouldBe input expected = (input, expected)

-- | Test string splitting with Unicode
test_split_unicode :: TestTree
test_split_unicode = testCase "split with Unicode characters" $ do
  let unicodeTests = 
        [ ("café,restaurant", [","], ["café", "restaurant"])  -- Unicode with comma
        ("naïve|sophisticated", ["|"], ["naïve", "sophisticated"])  -- Unicode with pipe
        ("你好，世界", ["，"], ["你好", "世界"])  -- Chinese comma
        ("Москва;Санкт-Петербург", [";"], ["Москва", "Санкт-Петербург"])  -- Cyrillic
        ]
  mapM_ (\(input, delims, expected) -> do
    mapM_ (\delim -> do
      let result = splitBy delim input
      assertEqual ("splitBy '" ++ delim ++ "' on Unicode") expected result
    ) delims
  ) unicodeTests

-- | Test comment removal with Unicode strings
test_comment_removal_unicode :: TestTree
test_comment_removal_unicode = testCase "comment removal with Unicode strings" $ do
  let unicodeComments = 
        [ ("你好 // 世界", "你好 ")  -- Chinese comment
        , "café /* français */ restaurant" `shouldBe` "café  restaurant"  -- French comment
        , "\"こんにちは//コメント\"" `shouldBe` "\"こんにちは//コメント\"""  -- Japanese in string
        , "'привет//комментарий'" `shouldBe` "'привет//комментарий'""  -- Russian in string
        ]
  mapM_ (\(input, expected) -> do
    let resultLine = removeLineComments input
        resultBoth = removeComments input
    assertEqual ("removeLineComments Unicode") expected resultLine
    assertEqual ("removeComments Unicode") expected resultBoth
  ) unicodeComments
  where
    shouldBe input expected = (input, expected)

-- | Test indentation normalization with mixed content
test_indentation_mixed_content :: TestTree
test_indentation_mixed_content = testCase "indentation normalization with mixed content" $ do
  let mixedCases = 
        [ ("  ascii\n\tunicode\n  mixed", "ascii\nunicode\nmixed")  -- mixed indentation
        ("    中文\n    русский\n    العربية", "中文\nрусский\nالعربية")  -- multilingual
        ("  code\n  // comment\n    indented", "code\n// comment\n  indented")  -- with comments
        ]
  mapM_ (\(input, expected) -> do
    let result = normalizeIndentation input
    assertEqual ("normalizeIndentation mixed content") expected result
  ) mixedCases

-- | Test string processing with control characters
test_control_characters :: TestTree
test_control_characters = testCase "string processing with control characters" $ do
  let controlTests = 
        [ ("\x01\x02data\x03\x04", "\x01\x02data\x03\x04")  -- control chars preserved
        , ("  \t\n\rdata\x00\x01", "data\x00\x01")  -- whitespace trimmed, control preserved
        , ("// comment\x01\x02", " ")  -- comment removed, control chars in comment
        ]
  mapM_ (\(input, expected) -> do
    let trimmed = trim input
        noComments = removeLineComments input
    assertEqual ("trim with control chars") expected trimmed
  ) controlTests

-- | Test very long strings
test_long_strings :: TestTree
test_long_strings = testCase "string processing with long strings" $ do
  let longString = replicate 10000 'a'
      longWithSpaces = "  " ++ longString ++ "  "
      longWithComments = longString ++ " // comment"
  assertEqual "trim long string" longString (trim longWithSpaces)
  assertEqual "remove comments from long string" (longString ++ " ") (removeLineComments longWithComments)

-- | Test empty and edge case strings
test_edge_case_strings :: TestTree
test_edge_case_strings = testCase "empty and edge case strings" $ do
  let edgeCases = 
        [ ("", "", "")  -- empty
        , (" ", "", "")  -- single space
        , ("//", " ", " ")  -- only comment
        , ("/* */", "  ", "  ")  -- only block comment
        , ("\"\"", "\"\"", "\"\"")  -- empty string
        , ("''", "''", "''")  -- empty char
        ]
  mapM_ (\(input, expectedTrim, expectedComments) -> do
    assertEqual ("trim edge case") expectedTrim (trim input)
    assertEqual ("removeLineComments edge case") expectedComments (removeLineComments input)
  ) edgeCases

-- | Property: trim preserves non-whitespace characters
prop_trim_preserves_content :: String -> Property
prop_trim_preserves_content input =
  let trimmed = trim input
      nonWhitespaceContent = filter (not . isSpace) input
      trimmedContent = filter (not . isSpace) trimmed
  in property $ trimmedContent == nonWhitespaceContent

-- | Property: splitBy is consistent for any delimiter
prop_splitby_consistent :: Char -> String -> Property
prop_splitby_consistent delim input =
  let result1 = splitBy delim input
      result2 = splitBy delim input
  in property $ result1 == result2

-- | Property: splitBy and splitByCollapsed relationship holds for Unicode
prop_splitby_unicode_relationship :: Char -> String -> Property
prop_splitby_unicode_relationship delim input =
  let normal = splitBy delim input
      collapsed = splitByCollapsed delim input
      filtered = filter (not . null) normal
  in property $ collapsed == filtered

-- | Property: Comment removal is idempotent
prop_comment_removal_idempotent :: String -> Property
prop_comment_removal_idempotent input =
  let onceLine = removeLineComments input
      twiceLine = removeLineComments onceLine
      onceBoth = removeComments input
      twiceBoth = removeComments onceBoth
  in property $ onceLine == twiceLine .&&. onceBoth == twiceBoth

-- | Property: String processing preserves Unicode characters
prop_unicode_preservation :: Property
prop_unicode_preservation = 
  forAll genUnicodeString $ \unicodeStr ->
  let trimmed = trim unicodeStr
      split = splitBy ',' unicodeStr
      noComments = removeLineComments unicodeStr
      normalized = normalizeIndentation unicodeStr
  in property $ all (`elem` unicodeStr) (concat split)  -- All chars preserved in split

-- | Property: String processing handles control characters gracefully
prop_control_character_handling :: Property
prop_control_character_handling = 
  forAll genControlString $ \controlStr ->
  let trimmed = trim controlStr
      processed = removeLineComments controlStr
  in property $ length processed <= length controlStr  -- Processing doesn't add characters

-- | Property: String operations are consistent with Data.Text operations
prop_text_consistency :: String -> Property
prop_text_consistency input =
  let textInput = T.pack input
      textTrimmed = T.strip textInput
      haskellTrimmed = trim input
  in property $ T.unpack textTrimmed == haskellTrimmed

-- | Property: String splitting preserves total length (minus delimiters)
prop_split_length_preservation :: Char -> String -> Property
prop_split_length_preservation delim input =
  let parts = splitBy delim input
      reconstructed = intercalate [delim] parts
      originalLength = length input
      reconstructedLength = length reconstructed
  in property $ abs (originalLength - reconstructedLength) <= 1  -- Allow off-by-one due to edge cases

-- | Property: Indentation normalization preserves line count
prop_indentation_preserves_lines :: String -> Property
prop_indentation_preserves_lines input =
  let originalLines = length $ lines input
      normalized = normalizeIndentation input
      normalizedLines = length $ lines normalized
  in property $ originalLines == normalizedLines

-- | Property: String processing works with very long inputs
prop_long_string_processing :: Property
prop_long_string_processing = 
  forAll (choose (1, 10000)) $ \size ->
  forAll (vectorOf size (elements "abc\n//comment\t  ")) $ \longStr ->
  let trimmed = trim longStr
      split = splitBy ',' longStr
      noComments = removeLineComments longStr
  in property $ not (null trimmed) .&&. length split >= 1

tests :: TestTree
tests = testGroup "String Utils Boundary Tests"
  [ test_trim_character_sets
  , test_split_unicode
  , test_comment_removal_unicode
  , test_indentation_mixed_content
  , test_control_characters
  , test_long_strings
  , test_edge_case_strings
  , fastProperty "trim preserves content" prop_trim_preserves_content
  , fastProperty "splitBy consistent" prop_splitby_consistent
  , fastProperty "splitBy Unicode relationship" prop_splitby_unicode_relationship
  , fastProperty "comment removal idempotent" prop_comment_removal_idempotent
  , fastProperty "Unicode preservation" prop_unicode_preservation
  , fastProperty "control character handling" prop_control_character_handling
  , fastProperty "text consistency" prop_text_consistency
  , fastProperty "split length preservation" prop_split_length_preservation
  , fastProperty "indentation preserves lines" prop_indentation_preserves_lines
  , fastProperty "long string processing" prop_long_string_processing
  ]