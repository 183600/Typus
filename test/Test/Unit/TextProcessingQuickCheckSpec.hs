module Test.Unit.TextProcessingQuickCheckSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import TestSupport.QuickCheck (fastProperty)
import Data.Char (isSpace, toUpper, toLower)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf)
import qualified Data.Text as T

-- Properties for string length
prop_length_reverse :: String -> Bool
prop_length_reverse s = length s == length (reverse s)

prop_length_append :: String -> String -> Bool
prop_length_append s1 s2 = length (s1 ++ s2) == length s1 + length s2

-- Properties for string operations
prop_reverse_reverse :: String -> Bool
prop_reverse_reverse s = reverse (reverse s) == s

prop_append_associative :: String -> String -> String -> Bool
prop_append_associative s1 s2 s3 = (s1 ++ s2) ++ s3 == s1 ++ (s2 ++ s3)

prop_append_identity_left :: String -> Bool
prop_append_identity_left s = "" ++ s == s

prop_append_identity_right :: String -> Bool
prop_append_identity_right s = s ++ "" == s

-- Properties for character transformations
prop_upper_preserves_length :: String -> Bool
prop_upper_preserves_length s = length (map toUpper s) == length s

prop_lower_preserves_length :: String -> Bool
prop_lower_preserves_length s = length (map toLower s) == length s

prop_upper_lower_roundtrip :: String -> Bool
prop_upper_lower_roundtrip s = map toLower (map toUpper s) == map toLower s

-- Properties for substring operations
prop_prefix_of_self :: String -> Bool
prop_prefix_of_self s = isPrefixOf s s

prop_suffix_of_self :: String -> Bool
prop_suffix_of_self s = isSuffixOf s s

prop_infix_of_self :: String -> Bool
prop_infix_of_self s = isInfixOf s s

-- Properties for whitespace
prop_space_is_space :: Char -> Property
prop_space_is_space c = isSpace c ==> (isSpace c)

-- Properties for text operations
prop_text_length_preserved :: String -> Bool
prop_text_length_preserved s = T.length (T.pack s) == length s

prop_text_unpack_pack :: String -> Bool
prop_text_unpack_pack s = T.unpack (T.pack s) == s

-- Properties for word operations
prop_word_count_non_negative :: String -> Bool
prop_word_count_non_negative s = length (words s) >= 0

prop_line_count_non_negative :: String -> Bool
prop_line_count_non_negative s = length (lines s) >= 0

tests :: TestTree
tests = testGroup "Test.Unit.TextProcessingQuickCheckSpec Tests"
  [ fastProperty "length reverse" prop_length_reverse
  , fastProperty "length append" prop_length_append
  , fastProperty "reverse reverse" prop_reverse_reverse
  , fastProperty "append associative" prop_append_associative
  , fastProperty "append identity left" prop_append_identity_left
  , fastProperty "append identity right" prop_append_identity_right
  , fastProperty "upper preserves length" prop_upper_preserves_length
  , fastProperty "lower preserves length" prop_lower_preserves_length
  , fastProperty "upper lower roundtrip" prop_upper_lower_roundtrip
  , fastProperty "prefix of self" prop_prefix_of_self
  , fastProperty "suffix of self" prop_suffix_of_self
  , fastProperty "infix of self" prop_infix_of_self
  , fastProperty "space is space" prop_space_is_space
  , fastProperty "text length preserved" prop_text_length_preserved
  , fastProperty "text unpack pack" prop_text_unpack_pack
  , fastProperty "word count non negative" prop_word_count_non_negative
  , fastProperty "line count non negative" prop_line_count_non_negative
  ]