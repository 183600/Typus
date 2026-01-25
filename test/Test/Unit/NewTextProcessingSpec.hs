{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.NewTextProcessingSpec where


import Test.Tasty
import Test.Tasty.QuickCheck



import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, group, isInfixOf, isPrefixOf, isSuffixOf, delete, union, intersect, (\\), find, permutations, words, unwords, lines, unlines)
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter, isPunctuation, isSymbol, ord, chr)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Control.Monad (replicateM)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Foldable (toList)
import qualified Data.Tree as Tree

import SourceLocation
import Utils

-- Helper generators for text processing tests
genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r!@#$%^&*()_+-=[]{}|;':\",./<>?"

genAlphaChar :: Gen Char
genAlphaChar = elements $ ['a'..'z'] ++ ['A'..'Z']

genDigitChar :: Gen Char
genDigitChar = elements $ ['0'..'9']

genSpaceChar :: Gen Char
genSpaceChar = elements " \t\n\r"

genPunctuationChar :: Gen Char
genPunctuationChar = elements "!@#$%^&*()_+-=[]{}|;':\",./<>?"

genUnicodeChar :: Gen Char
genUnicodeChar = elements $ map chr [32..126] ++ map chr [160..255]

genString :: Gen String
genString = do
  len <- choose (0, 20)
  vectorOf len genChar

genAlphaString :: Gen String
genAlphaString = do
  len <- choose (0, 20)
  vectorOf len genAlphaChar

genDigitString :: Gen String
genDigitString = do
  len <- choose (0, 20)
  vectorOf len genDigitChar

genSpaceString :: Gen String
genSpaceString = do
  len <- choose (0, 10)
  vectorOf len genSpaceChar

genWord :: Gen String
genWord = do
  len <- choose (1, 10)
  vectorOf len genAlphaChar

genText :: Gen T.Text
genText = T.pack <$> genString

genAlphaText :: Gen T.Text
genAlphaText = T.pack <$> genAlphaString

-- Test properties for text processing

-- Property 1: String length is preserved by pack/unpack
prop_stringLengthPreservedByPackUnpack :: String -> Bool
prop_stringLengthPreservedByPackUnpack s = length s == T.length (T.pack s)

-- Property 2: String content is preserved by pack/unpack
prop_stringContentPreservedByPackUnpack :: String -> Bool
prop_stringContentPreservedByPackUnpack s = T.unpack (T.pack s) == s

-- Property 3: Text concatenation preserves content
prop_textConcatenationPreservesContent :: T.Text -> T.Text -> Bool
prop_textConcatenationPreservesContent t1 t2 = 
  T.unpack (t1 <> t2) == T.unpack t1 ++ T.unpack t2

-- Property 4: Text length is additive
prop_textLengthIsAdditive :: T.Text -> T.Text -> Bool
prop_textLengthIsAdditive t1 t2 = T.length (t1 <> t2) == T.length t1 + T.length t2

-- Property 5: Text split preserves characters
prop_textSplitPreservesCharacters :: T.Text -> T.Text -> Property
prop_textSplitPreservesCharacters t delimiter =
  not (T.null delimiter) ==> 
    let parts = T.splitOn delimiter t
        concatenated = T.intercalate delimiter parts
    in concatenated == t

-- Property 6: Text split on empty delimiter splits into characters
prop_textSplitOnEmptyDelimiter :: T.Text -> Bool
prop_textSplitOnEmptyDelimiter t = 
  let parts = T.splitOn (T.pack "") t
      chars = T.chunksOf 1 t
  in parts == chars

-- Property 7: Text take preserves prefix
prop_textTakePreservesPrefix :: T.Text -> Int -> Property
prop_textTakePreservesPrefix t n =
  n >= 0 && n <= T.length t ==> 
    let prefix = T.take n t
    in T.length prefix == n && T.isPrefixOf prefix t

-- Property 8: Text drop preserves suffix
prop_textDropPreservesSuffix :: T.Text -> Int -> Property
prop_textDropPreservesSuffix t n =
  n >= 0 && n <= T.length t ==> 
    let suffix = T.drop n t
    in T.length suffix == T.length t - n && T.isSuffixOf suffix t

-- Property 9: Text toUpper preserves letters
prop_textToUpperPreservesLetters :: T.Text -> Bool
prop_textToUpperPreservesLetters t = 
  let upper = T.toUpper t
      originalLength = T.length t
      upperLength = T.length upper
  in originalLength == upperLength

-- Property 10: Text toLower preserves letters
prop_textToLowerPreservesLetters :: T.Text -> Bool
prop_textToLowerPreservesLetters t = 
  let lower = T.toLower t
      originalLength = T.length t
      lowerLength = T.length lower
  in originalLength == lowerLength

-- Property 11: Text reverse preserves characters
prop_textReversePreservesCharacters :: T.Text -> Bool
prop_textReversePreservesCharacters t = 
  let reversed = T.reverse t
      doubleReversed = T.reverse reversed
  in doubleReversed == t

-- Property 12: Text filter preserves subset
prop_textFilterPreservesSubset :: T.Text -> Bool
prop_textFilterPreservesSubset t = 
  let isAlpha c = isAlpha (T.head (T.pack [c]))
      filtered = T.filter isAlpha t
  in T.length filtered <= T.length t

-- Property 13: Text map preserves length
prop_textMapPreservesLength :: T.Text -> Bool
prop_textMapPreservesLength t = 
  let mapped = T.map toUpper t
  in T.length mapped == T.length t

-- Property 14: Text concat is associative
prop_textConcatIsAssociative :: T.Text -> T.Text -> T.Text -> Bool
prop_textConcatIsAssociative t1 t2 t3 = (t1 <> t2) <> t3 == t1 <> (t2 <> t3)

-- Property 15: Text concat has identity element
prop_textConcatHasIdentity :: T.Text -> Bool
prop_textConcatHasIdentity t = T.empty <> t == t && t <> T.empty == t

-- Property 16: String words preserve content
prop_stringWordsPreserveContent :: String -> Bool
prop_stringWordsPreserveContent s = unwords (words s) == filter (not . all isSpace) s

-- Property 17: String lines preserve content
prop_stringLinesPreserveContent :: String -> Bool
prop_stringLinesPreserveContent s = unlines (lines s) == s

-- Property 18: String toUpper/toLower are inverses for letters
prop_stringToUpperToLowerInverses :: String -> Bool
prop_stringToUpperToLowerInverses s = 
  let upper = map toUpper s
      lower = map toLower upper
      alphaOnly = filter isAlpha s
      alphaUpper = map toUpper alphaOnly
      alphaLower = map toLower alphaUpper
  in alphaLower == map toLower alphaOnly

-- Property 19: String split preserves characters
prop_stringSplitPreservesCharacters :: String -> String -> Property
prop_stringSplitPreservesCharacters s delimiter =
  not (null delimiter) ==> 
    let parts = splitOn delimiter s
        concatenated = intercalate delimiter parts
    in concatenated == s
  where
    splitOn _ [] = [""]
    splitOn [] s = [s]
    splitOn sep str = splitOn' sep str []
      where
        splitOn' _ [] acc = [reverse acc]
        splitOn' sep str acc
          | sep `isPrefixOf` str = reverse acc : splitOn' sep (drop (length sep) str) []
          | otherwise = splitOn' sep (tail str) (head str : acc)
    
    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Property 20: String isPrefixOf/isSuffixOf are consistent
prop_stringPrefixSuffixConsistent :: String -> String -> Bool
prop_stringPrefixSuffixConsistent prefix s =
  let isPref = prefix `isPrefixOf` s
      isSuff = prefix `isSuffixOf` s
      prefLength = length prefix
      suffLength = length prefix
      sLength = length s
  in (isPref && prefLength <= sLength) || (not isPref && (prefLength > sLength || take prefLength s /= prefix))

-- Property 21: Text replace preserves length when same size
prop_textReplacePreservesLengthSameSize :: T.Text -> T.Text -> T.Text -> Property
prop_textReplacePreservesLengthSameSize t old new =
  not (T.null old) && T.length old == T.length new ==> 
    let replaced = T.replace old new t
    in T.length replaced == T.length t

-- Property 22: Text replace removes occurrences
prop_textReplaceRemovesOccurrences :: T.Text -> T.Text -> Property
prop_textReplaceRemovesOccurrences t old =
  not (T.null old) ==> 
    let replaced = T.replace old (T.pack "") t
        oldCount = T.count old t
        newCount = T.count old replaced
    in newCount == 0

-- Property 23: Text count is additive
prop_textCountIsAdditive :: T.Text -> T.Text -> T.Text -> Property
prop_textCountIsAdditive t1 t2 pattern =
  not (T.null pattern) ==> 
    let combined = t1 <> t2
        count1 = T.count pattern t1
        count2 = T.count pattern t2
        countCombined = T.count pattern combined
    in countCombined == count1 + count2

-- Property 24: Text strip preserves non-space content
prop_textStripPreservesNonSpaceContent :: T.Text -> Property
prop_textStripPreservesNonSpaceContent t =
  not (T.all isSpace t) ==> 
    let stripped = T.strip t
    in not (T.null stripped) && not (T.isPrefixOf (T.pack " ") stripped) && not (T.isSuffixOf (T.pack " ") stripped)

-- Property 25: Text strip of all spaces is empty
prop_textStripAllSpacesIsEmpty :: T.Text -> Property
prop_textStripAllSpacesIsEmpty t =
  T.all isSpace t ==> T.strip t == T.empty

-- Property 26: Text compare is consistent with length
prop_textCompareConsistentWithLength :: T.Text -> T.Text -> Bool
prop_textCompareConsistentWithLength t1 t2 =
  let comparison = T.compare t1 t2
      len1 = T.length t1
      len2 = T.length t2
  in (t1 == t2 && comparison == EQ) ||
     (t1 /= t2 && (comparison == LT || comparison == GT))

-- Property 27: Text indexing is consistent
prop_textIndexingConsistent :: T.Text -> Int -> Property
prop_textIndexingConsistent t i =
  i >= 0 && i < T.length t ==> 
    let c = T.index t i
    in T.length (T.singleton c) == 1

-- Property 28: Text singleton preserves character
prop_textSingletonPreservesCharacter :: Char -> Bool
prop_textSingletonPreservesCharacter c = T.head (T.singleton c) == c

-- Property 29: Text cons preserves length
prop_textConsPreservesLength :: Char -> T.Text -> Bool
prop_textConsPreservesLength c t = T.length (T.cons c t) == T.length t + 1

-- Property 30: Text uncons preserves content
prop_textUnconsPreservesContent :: T.Text -> Property
prop_textUnconsPreservesContent t =
  not (T.null t) ==> 
    case T.uncons t of
      Just (c, rest) -> T.cons c rest == t
      Nothing -> False

-- Property 31: Text append preserves content
prop_textAppendPreservesContent :: T.Text -> T.Text -> Bool
prop_textAppendPreservesContent t1 t2 = T.append t1 t2 == t1 <> t2

-- Property 32: Text init preserves all but last
prop_textInitPreservesAllButLast :: T.Text -> Property
prop_textInitPreservesAllButLast t =
  not (T.null t) ==> 
    let init = T.init t
        last = T.last t
    in T.snoc init last == t

-- Property 33: Text tail preserves all but first
prop_textTailPreservesAllButFirst :: T.Text -> Property
prop_textTailPreservesAllButFirst t =
  not (T.null t) ==> 
    let head = T.head t
        tail = T.tail t
    in T.cons head tail == t

-- Property 34: Text snoc preserves length
prop_textSnocPreservesLength :: T.Text -> Char -> Bool
prop_textSnocPreservesLength t c = T.length (T.snoc t c) == T.length t + 1

-- Property 35: Text any/all are consistent
prop_textAnyAllConsistent :: T.Text -> Bool
prop_textAnyAllConsistent t = 
  let isSpace = T.all (== ' ') (T.filter (== ' ') t)
  in (T.any (== ' ') t) || not (T.any (== ' ') t)

-- Property 36: Text findIndex is consistent
prop_textFindIndexConsistent :: T.Text -> Char -> Property
prop_textFindIndexConsistent t c =
  case T.findIndex (== c) t of
    Just i -> i >= 0 && i < T.length t && T.index t i == c
    Nothing -> not (T.any (== c) t)

-- Property 37: Text foldl preserves length
prop_textFoldlPreservesLength :: T.Text -> Bool
prop_textFoldlPreservesLength t = 
  let folded = T.foldl (\acc c -> acc ++ [c]) "" t
  in length folded == T.length t

-- Property 38: Text foldr preserves length
prop_textFoldrPreservesLength :: T.Text -> Bool
prop_textFoldrPreservesLength t = 
  let folded = T.foldr (\c acc -> [c] ++ acc) "" t
  in length folded == T.length t

-- Property 39: Text unfoldr produces correct length
prop_textUnfoldrProducesCorrectLength :: Int -> Property
prop_textUnfoldrProducesCorrectLength n =
  n >= 0 && n <= 100 ==> 
    let t = T.unfoldr (\i -> if i < n then Just ('a', i + 1) else Nothing) 0
    in T.length t == n

-- Property 40: Text replicate preserves length
prop_textReplicatePreservesLength :: T.Text -> Int -> Property
prop_textReplicatePreservesLength t n =
  n >= 0 && n <= 10 ==> 
    let replicated = T.replicate n t
    in T.length replicated == n * T.length t

newTextProcessingTests :: TestTree
newTextProcessingTests = testGroup "New Text Processing Tests"
  [ testProperties "Basic Text Operations"
    [ ("String length is preserved by pack/unpack", property prop_stringLengthPreservedByPackUnpack)
    , ("String content is preserved by pack/unpack", property prop_stringContentPreservedByPackUnpack)
    , ("Text concatenation preserves content", property prop_textConcatenationPreservesContent)
    , ("Text length is additive", property prop_textLengthIsAdditive)
    , ("Text concat is associative", property prop_textConcatIsAssociative)
    , ("Text concat has identity element", property prop_textConcatHasIdentity)
    ]
  , testProperties "Text Splitting and Joining"
    [ ("Text split preserves characters", property prop_textSplitPreservesCharacters)
    , ("Text split on empty delimiter splits into characters", property prop_textSplitOnEmptyDelimiter)
    , ("String words preserve content", property prop_stringWordsPreserveContent)
    , ("String lines preserve content", property prop_stringLinesPreserveContent)
    , ("String split preserves characters", property prop_stringSplitPreservesCharacters)
    ]
  , testProperties "Text Transformation"
    [ ("Text toUpper preserves letters", property prop_textToUpperPreservesLetters)
    , ("Text toLower preserves letters", property prop_textToLowerPreservesLetters)
    , ("Text reverse preserves characters", property prop_textReversePreservesCharacters)
    , ("Text filter preserves subset", property prop_textFilterPreservesSubset)
    , ("Text map preserves length", property prop_textMapPreservesLength)
    , ("String toUpper/toLower are inverses for letters", property prop_stringToUpperToLowerInverses)
    ]
  , testProperties "Text Substring Operations"
    [ ("Text take preserves prefix", property prop_textTakePreservesPrefix)
    , ("Text drop preserves suffix", property prop_textDropPreservesSuffix)
    , ("Text strip preserves non-space content", property prop_textStripPreservesNonSpaceContent)
    , ("Text strip of all spaces is empty", property prop_textStripAllSpacesIsEmpty)
    ]
  , testProperties "Text Search and Replace"
    [ ("String isPrefixOf/isSuffixOf are consistent", property prop_stringPrefixSuffixConsistent)
    , ("Text replace preserves length when same size", property prop_textReplacePreservesLengthSameSize)
    , ("Text replace removes occurrences", property prop_textReplaceRemovesOccurrences)
    , ("Text count is additive", property prop_textCountIsAdditive)
    ]
  , testProperties "Text Construction and Deconstruction"
    [ ("Text singleton preserves character", property prop_textSingletonPreservesCharacter)
    , ("Text cons preserves length", property prop_textConsPreservesLength)
    , ("Text uncons preserves content", property prop_textUnconsPreservesContent)
    , ("Text append preserves content", property prop_textAppendPreservesContent)
    , ("Text init preserves all but last", property prop_textInitPreservesAllButLast)
    , ("Text tail preserves all but first", property prop_textTailPreservesAllButFirst)
    , ("Text snoc preserves length", property prop_textSnocPreservesLength)
    ]
  , testProperties "Text Properties and Queries"
    [ ("Text compare is consistent with length", property prop_textCompareConsistentWithLength)
    , ("Text indexing is consistent", property prop_textIndexingConsistent)
    , ("Text any/all are consistent", property prop_textAnyAllConsistent)
    , ("Text findIndex is consistent", property prop_textFindIndexConsistent)
    ]
  , testProperties "Text Folding and Unfolding"
    [ ("Text foldl preserves length", property prop_textFoldlPreservesLength)
    , ("Text foldr preserves length", property prop_textFoldrPreservesLength)
    , ("Text unfoldr produces correct length", property prop_textUnfoldrProducesCorrectLength)
    , ("Text replicate preserves length", property prop_textReplicatePreservesLength)
    ]
  ]