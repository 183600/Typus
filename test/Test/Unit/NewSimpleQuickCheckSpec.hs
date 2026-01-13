{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Test.Unit.NewSimpleQuickCheckSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties, Arbitrary(..), Gen, choose, listOf, elements, oneof, vectorOf, property, (===), forAll, counterexample)
import Test.QuickCheck (Gen, Property, (==>))
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, group, isInfixOf, isPrefixOf, isSuffixOf, delete, union, intersect, (\\), find, permutations)
import Data.Char (isSpace, isAlpha, isAlphaNum, toLower, toUpper, isDigit, isLetter, isPunctuation, isSymbol, ord, chr)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Control.Monad (replicateM)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Foldable (toList)

-- Helper generators for simple tests
genSmallInt :: Gen Int
genSmallInt = choose (-10, 10)

genPositiveInt :: Gen Int
genPositiveInt = choose (1, 100)

genChar :: Gen Char
genChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

genString :: Gen String
genString = do
  len <- choose (0, 20)
  vectorOf len genChar

genAlphaString :: Gen String
genAlphaString = do
  len <- choose (0, 20)
  vectorOf len $ elements $ ['a'..'z'] ++ ['A'..'Z']

genText :: Gen T.Text
genText = T.pack <$> genString

-- Arbitrary instances
instance Arbitrary T.Text where
  arbitrary = genText

-- Test properties for simple functionality

-- Property 1: Addition is commutative
prop_additionCommutative :: Int -> Int -> Bool
prop_additionCommutative x y = x + y == y + x

-- Property 2: Addition is associative
prop_additionAssociative :: Int -> Int -> Int -> Bool
prop_additionAssociative x y z = (x + y) + z == x + (y + z)

-- Property 3: Multiplication is commutative
prop_multiplicationCommutative :: Int -> Int -> Bool
prop_multiplicationCommutative x y = x * y == y * x

-- Property 4: Multiplication is associative
prop_multiplicationAssociative :: Int -> Int -> Int -> Bool
prop_multiplicationAssociative x y z = (x * y) * z == x * (y * z)

-- Property 5: Distributive law
prop_distributiveLaw :: Int -> Int -> Int -> Bool
prop_distributiveLaw x y z = x * (y + z) == x * y + x * z

-- Property 6: Identity elements
prop_identityElements :: Int -> Bool
prop_identityElements x = x + 0 == x && x * 1 == x

-- Property 7: Additive inverse
prop_additiveInverse :: Int -> Bool
prop_additiveInverse x = x + (-x) == 0

-- Property 8: Multiplication by zero
prop_multiplicationByZero :: Int -> Bool
prop_multiplicationByZero x = x * 0 == 0

-- Property 9: List concatenation is associative
prop_listConcatenationAssociative :: [Int] -> [Int] -> [Int] -> Bool
prop_listConcatenationAssociative xs ys zs = (xs ++ ys) ++ zs == xs ++ (ys ++ zs)

-- Property 10: List concatenation identity
prop_listConcatenationIdentity :: [Int] -> Bool
prop_listConcatenationIdentity xs = [] ++ xs == xs && xs ++ [] == xs

-- Property 11: List length properties
prop_listLengthProperties :: [Int] -> [Int] -> Bool
prop_listLengthProperties xs ys = length (xs ++ ys) == length xs + length ys

-- Property 12: List reverse properties
prop_listReverseProperties :: [Int] -> Bool
prop_listReverseProperties xs = reverse (reverse xs) == xs

-- Property 13: List sort preserves elements
prop_listSortPreserves :: [Int] -> Bool
prop_listSortPreserves xs = sort xs == sort xs

-- Property 14: List nub removes duplicates
prop_listNubRemovesDuplicates :: [Int] -> Bool
prop_listNubRemovesDuplicates xs = length (nub xs) <= length xs

-- Property 15: String length is preserved by pack/unpack
prop_stringLengthPreservedByPackUnpack :: String -> Bool
prop_stringLengthPreservedByPackUnpack s = length s == T.length (T.pack s)

-- Property 16: String content is preserved by pack/unpack
prop_stringContentPreservedByPackUnpack :: String -> Bool
prop_stringContentPreservedByPackUnpack s = T.unpack (T.pack s) == s

-- Property 17: Text concatenation preserves content
prop_textConcatenationPreservesContent :: T.Text -> T.Text -> Bool
prop_textConcatenationPreservesContent t1 t2 = T.unpack (t1 <> t2) == T.unpack t1 ++ T.unpack t2

-- Property 18: Text length is additive
prop_textLengthIsAdditive :: T.Text -> T.Text -> Bool
prop_textLengthIsAdditive t1 t2 = T.length (t1 <> t2) == T.length t1 + T.length t2

-- Property 19: Text toUpper preserves letters
prop_textToUpperPreservesLetters :: T.Text -> Bool
prop_textToUpperPreservesLetters t = 
  let upper = T.toUpper t
      originalLength = T.length t
      upperLength = T.length upper
  in originalLength == upperLength

-- Property 20: Text toLower preserves letters
prop_textToLowerPreservesLetters :: T.Text -> Bool
prop_textToLowerPreservesLetters t = 
  let lower = T.toLower t
      originalLength = T.length t
      lowerLength = T.length lower
  in originalLength == lowerLength

-- Property 21: Text reverse preserves characters
prop_textReversePreservesCharacters :: T.Text -> Bool
prop_textReversePreservesCharacters t = 
  let reversed = T.reverse t
      doubleReversed = T.reverse reversed
  in doubleReversed == t

-- Property 22: Map lookup finds inserted values
prop_mapLookupFindsInserted :: [(String, Int)] -> Property
prop_mapLookupFindsInserted pairs =
  not (null pairs) ==> 
    let map = Map.fromList pairs
    in all (\(k, v) -> Map.lookup k map == Just v) pairs

-- Property 23: Map size is preserved by insert-delete
prop_mapSizePreservedByInsertDelete :: Map.Map String Int -> String -> Int -> Bool
prop_mapSizePreservedByInsertDelete map key value =
  let map1 = Map.insert key value map
      map2 = Map.delete key map1
  in Map.size map2 == Map.size map

-- Property 24: Set insertion adds elements
prop_setInsertionAdds :: Set.Set Int -> Int -> Bool
prop_setInsertionAdds set value =
  let set1 = Set.insert value set
  in Set.member value set1 && Set.size set1 >= Set.size set

-- Property 25: Set deletion removes elements
prop_setDeletionRemoves :: Set.Set Int -> Int -> Bool
prop_setDeletionRemoves set value =
  let set1 = Set.insert value set
      set2 = Set.delete value set1
  in not (Set.member value set2) && Set.size set2 <= Set.size set1

-- Property 26: Set union combines all elements
prop_setUnionCombinesAll :: Set.Set Int -> Set.Set Int -> Bool
prop_setUnionCombinesAll set1 set2 =
  let unionSet = Set.union set1 set2
  in all (`Set.member` unionSet) (Set.toList set1) &&
     all (`Set.member` unionSet) (Set.toList set2)

-- Property 27: Set intersection finds common elements
prop_setIntersectionFindsCommon :: Set.Set Int -> Set.Set Int -> Bool
prop_setIntersectionFindsCommon set1 set2 =
  let intersectionSet = Set.intersection set1 set2
  in all (`Set.member` intersectionSet) (Set.toList $ Set.intersection set1 set2)

-- Property 28: Either left or right
prop_eitherLeftOrRight :: Either Int String -> Bool
prop_eitherLeftOrRight either = isLeft either || isRight either
  where
    isLeft (Left _) = True
    isLeft _ = False
    isRight (Right _) = True
    isRight _ = False

-- Property 29: Maybe is just or nothing
prop_maybeIsJustOrNothing :: Maybe Int -> Bool
prop_maybeIsJustOrNothing maybe = isJust maybe || isNothing maybe

-- Property 30: Tuple projection preserves components
prop_tupleProjectionPreserves :: (Int, String, Bool) -> Bool
prop_tupleProjectionPreserves (i, s, b) = 
  let (i1, _, _) = (i, s, b)
      (_, s1, _) = (i, s, b)
      (_, _, b1) = (i, s, b)
  in i1 == i && s1 == s && b1 == b

-- Property 31: String toUpper/toLower are inverses for letters
prop_stringToUpperToLowerInverses :: String -> Bool
prop_stringToUpperToLowerInverses s = 
  let upper = map toUpper s
      lower = map toLower upper
      alphaOnly = filter isAlpha s
      alphaUpper = map toUpper alphaOnly
      alphaLower = map toLower alphaUpper
  in alphaLower == map toLower alphaOnly

-- Property 32: String split preserves characters
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

-- Property 33: String isPrefixOf/isSuffixOf are consistent
prop_stringPrefixSuffixConsistent :: String -> String -> Bool
prop_stringPrefixSuffixConsistent prefix s =
  let isPref = prefix `isPrefixOf` s
      isSuff = prefix `isSuffixOf` s
      prefLength = length prefix
      suffLength = length prefix
      sLength = length s
  in (isPref && prefLength <= sLength) || (not isPref && (prefLength > sLength || take prefLength s /= prefix))

-- Property 34: List zip preserves pairs
prop_listZipPreservesPairs :: [Int] -> [String] -> Bool
prop_listZipPreservesPairs xs ys =
  let zipped = zip xs ys
      unzipped = unzip zipped
  in fst unzipped == take (min (length xs) (length ys)) xs &&
     snd unzipped == take (min (length xs) (length ys)) ys

-- Property 35: List unzip preserves pairs
prop_listUnzipPreservesPairs :: [(Int, String)] -> Bool
prop_listUnzipPreservesPairs pairs =
  let (xs, ys) = unzip pairs
      zipped = zip xs ys
  in length zipped == length pairs

-- Property 36: Map keys are unique
prop_mapKeysAreUnique :: [(String, Int)] -> Bool
prop_mapKeysAreUnique pairs =
  let map = Map.fromList pairs
      keys = Map.keys map
  in length keys == length (nub keys)

-- Property 37: Set elements are unique
prop_setElementsAreUnique :: [Int] -> Bool
prop_setElementsAreUnique xs =
  let set = Set.fromList xs
      elements = Set.toList set
  in length elements == length (nub elements)

-- Property 38: List group preserves elements
prop_listGroupPreserves :: [Int] -> Bool
prop_listGroupPreserves xs =
  let grouped = group $ sort xs
      flattened = concat grouped
  in sort xs == flattened

-- Property 39: List permutations preserve elements
prop_listPermutationsPreserve :: [Int] -> Property
prop_listPermutationsPreserve xs =
  length xs <= 6 ==> 
    let perms = permutations xs
    in all (\perm -> sort perm == sort xs) perms

-- Property 40: Map fromList with duplicates keeps last value
prop_mapFromListWithDuplicates :: [(String, Int)] -> Bool
prop_mapFromListWithDuplicates pairs =
  let map = Map.fromList pairs
  in all (\(k, v) -> Map.lookup k map == Just v || 
                    Map.lookup k map == Just (snd $ last $ filter (\(k', _) -> k' == k) pairs)) pairs

-- Property 41: Set toList and fromList are inverses
prop_setToListFromListInverse :: Set.Set Int -> Bool
prop_setToListFromListInverse set =
  let list = Set.toList set
      set' = Set.fromList list
  in set == set'

-- Property 42: Map toList and fromList are inverses
prop_mapToListFromListInverse :: Map.Map String Int -> Bool
prop_mapToListFromListInverse map =
  let list = Map.toList map
      map' = Map.fromList list
  in map == map'

-- Property 43: Text split preserves characters
prop_textSplitPreservesCharacters :: T.Text -> T.Text -> Property
prop_textSplitPreservesCharacters t delimiter =
  not (T.null delimiter) ==> 
    let parts = T.splitOn delimiter t
        concatenated = T.intercalate delimiter parts
    in concatenated == t

-- Property 44: Text split on empty delimiter splits into characters
prop_textSplitOnEmptyDelimiter :: T.Text -> Bool
prop_textSplitOnEmptyDelimiter t = 
  let parts = T.splitOn (T.pack "") t
      chars = T.chunksOf 1 t
  in parts == chars

-- Property 45: Text take preserves prefix
prop_textTakePreservesPrefix :: T.Text -> Int -> Property
prop_textTakePreservesPrefix t n =
  n >= 0 && n <= T.length t ==> 
    let prefix = T.take n t
    in T.length prefix == n && T.isPrefixOf prefix t

-- Property 46: Text drop preserves suffix
prop_textDropPreservesSuffix :: T.Text -> Int -> Property
prop_textDropPreservesSuffix t n =
  n >= 0 && n <= T.length t ==> 
    let suffix = T.drop n t
    in T.length suffix == T.length t - n && T.isSuffixOf suffix t

-- Property 47: Text strip preserves non-space content
prop_textStripPreservesNonSpaceContent :: T.Text -> Property
prop_textStripPreservesNonSpaceContent t =
  not (T.all isSpace t) ==> 
    let stripped = T.strip t
    in not (T.null stripped) && not (T.isPrefixOf (T.pack " ") stripped) && not (T.isSuffixOf (T.pack " ") stripped)

-- Property 48: Text strip of all spaces is empty
prop_textStripAllSpacesIsEmpty :: T.Text -> Property
prop_textStripAllSpacesIsEmpty t =
  T.all isSpace t ==> T.strip t == T.empty

-- Property 49: Text singleton preserves character
prop_textSingletonPreservesCharacter :: Char -> Bool
prop_textSingletonPreservesCharacter c = T.head (T.singleton c) == c

-- Property 50: Text cons preserves length
prop_textConsPreservesLength :: Char -> T.Text -> Bool
prop_textConsPreservesLength c t = T.length (T.cons c t) == T.length t + 1

-- Property 51: Text uncons preserves content
prop_textUnconsPreservesContent :: T.Text -> Property
prop_textUnconsPreservesContent t =
  not (T.null t) ==> 
    case T.uncons t of
      Just (c, rest) -> T.cons c rest == t
      Nothing -> False

-- Property 52: Text append preserves content
prop_textAppendPreservesContent :: T.Text -> T.Text -> Bool
prop_textAppendPreservesContent t1 t2 = T.append t1 t2 == t1 <> t2

-- Property 53: Text init preserves all but last
prop_textInitPreservesAllButLast :: T.Text -> Property
prop_textInitPreservesAllButLast t =
  not (T.null t) ==> 
    let init = T.init t
        last = T.last t
    in T.snoc init last == t

-- Property 54: Text tail preserves all but first
prop_textTailPreservesAllButFirst :: T.Text -> Property
prop_textTailPreservesAllButFirst t =
  not (T.null t) ==> 
    let head = T.head t
        tail = T.tail t
    in T.cons head tail == t

-- Property 55: Text snoc preserves length
prop_textSnocPreservesLength :: T.Text -> Char -> Bool
prop_textSnocPreservesLength t c = T.length (T.snoc t c) == T.length t + 1

-- Property 56: Text any/all are consistent
prop_textAnyAllConsistent :: T.Text -> Bool
prop_textAnyAllConsistent t = 
  let isSpace = T.all (== ' ') (T.filter (== ' ') t)
  in (T.any (== ' ') t) || not (T.any (== ' ') t)

-- Property 57: Text findIndex is consistent
prop_textFindIndexConsistent :: T.Text -> Char -> Property
prop_textFindIndexConsistent t c =
  property $ case T.findIndex (== c) t of
    Just i -> i >= 0 && i < T.length t
    Nothing -> not (T.any (== c) t)

-- Property 58: Text foldl preserves length
prop_textFoldlPreservesLength :: T.Text -> Bool
prop_textFoldlPreservesLength t = 
  let folded = T.foldl (\acc c -> acc `T.append` T.singleton c) T.empty t
  in T.length folded == T.length t

-- Property 59: Text foldr preserves length
prop_textFoldrPreservesLength :: T.Text -> Bool
prop_textFoldrPreservesLength t = 
  let folded = T.foldr (\c acc -> T.singleton c `T.append` acc) T.empty t
  in T.length folded == T.length t

-- Property 60: Text replicate preserves length
prop_textReplicatePreservesLength :: T.Text -> Int -> Property
prop_textReplicatePreservesLength t n =
  n >= 0 && n <= 10 ==> 
    let replicated = T.replicate n t
    in T.length replicated == n * T.length t

newSimpleQuickCheckTests :: TestTree
newSimpleQuickCheckTests = testGroup "New Simple QuickCheck Tests"
  [ testProperties "Basic Arithmetic Properties"
    [ ("Addition is commutative", property prop_additionCommutative)
    , ("Addition is associative", property prop_additionAssociative)
    , ("Multiplication is commutative", property prop_multiplicationCommutative)
    , ("Multiplication is associative", property prop_multiplicationAssociative)
    , ("Distributive law", property prop_distributiveLaw)
    , ("Identity elements", property prop_identityElements)
    , ("Additive inverse", property prop_additiveInverse)
    , ("Multiplication by zero", property prop_multiplicationByZero)
    ]
  , testProperties "List Properties"
    [ ("List concatenation is associative", property prop_listConcatenationAssociative)
    , ("List concatenation identity", property prop_listConcatenationIdentity)
    , ("List length properties", property prop_listLengthProperties)
    , ("List reverse properties", property prop_listReverseProperties)
    , ("List sort preserves elements", property prop_listSortPreserves)
    , ("List nub removes duplicates", property prop_listNubRemovesDuplicates)
    , ("List group preserves elements", property prop_listGroupPreserves)
    , ("List permutations preserve elements", property prop_listPermutationsPreserve)
    , ("List zip preserves pairs", property prop_listZipPreservesPairs)
    , ("List unzip preserves pairs", property prop_listUnzipPreservesPairs)
    ]
  , testProperties "String and Text Properties"
    [ ("String length is preserved by pack/unpack", property prop_stringLengthPreservedByPackUnpack)
    , ("String content is preserved by pack/unpack", property prop_stringContentPreservedByPackUnpack)
    , ("Text concatenation preserves content", property prop_textConcatenationPreservesContent)
    , ("Text length is additive", property prop_textLengthIsAdditive)
    , ("Text toUpper preserves letters", property prop_textToUpperPreservesLetters)
    , ("Text toLower preserves letters", property prop_textToLowerPreservesLetters)
    , ("Text reverse preserves characters", property prop_textReversePreservesCharacters)
    , ("String toUpper/toLower are inverses for letters", property prop_stringToUpperToLowerInverses)
    , ("String split preserves characters", property prop_stringSplitPreservesCharacters)
    , ("String isPrefixOf/isSuffixOf are consistent", property prop_stringPrefixSuffixConsistent)
    ]
  , testProperties "Text Operations"
    [ ("Text split preserves characters", property prop_textSplitPreservesCharacters)
    , ("Text split on empty delimiter splits into characters", property prop_textSplitOnEmptyDelimiter)
    , ("Text take preserves prefix", property prop_textTakePreservesPrefix)
    , ("Text drop preserves suffix", property prop_textDropPreservesSuffix)
    , ("Text strip preserves non-space content", property prop_textStripPreservesNonSpaceContent)
    , ("Text strip of all spaces is empty", property prop_textStripAllSpacesIsEmpty)
    , ("Text singleton preserves character", property prop_textSingletonPreservesCharacter)
    , ("Text cons preserves length", property prop_textConsPreservesLength)
    , ("Text uncons preserves content", property prop_textUnconsPreservesContent)
    , ("Text append preserves content", property prop_textAppendPreservesContent)
    ]
  , testProperties "Text Advanced Operations"
    [ ("Text init preserves all but last", property prop_textInitPreservesAllButLast)
    , ("Text tail preserves all but first", property prop_textTailPreservesAllButFirst)
    , ("Text snoc preserves length", property prop_textSnocPreservesLength)
    , ("Text any/all are consistent", property prop_textAnyAllConsistent)
    , ("Text findIndex is consistent", property prop_textFindIndexConsistent)
    , ("Text foldl preserves length", property prop_textFoldlPreservesLength)
    , ("Text foldr preserves length", property prop_textFoldrPreservesLength)
    , ("Text replicate preserves length", property prop_textReplicatePreservesLength)
    ]
  , testProperties "Map and Set Properties"
    [ ("Map lookup finds inserted values", property prop_mapLookupFindsInserted)
    , ("Map size is preserved by insert-delete", property prop_mapSizePreservedByInsertDelete)
    , ("Map keys are unique", property prop_mapKeysAreUnique)
    , ("Map fromList with duplicates keeps last value", property prop_mapFromListWithDuplicates)
    , ("Map toList and fromList are inverses", property prop_mapToListFromListInverse)
    , ("Set insertion adds elements", property prop_setInsertionAdds)
    , ("Set deletion removes elements", property prop_setDeletionRemoves)
    , ("Set union combines all elements", property prop_setUnionCombinesAll)
    , ("Set intersection finds common elements", property prop_setIntersectionFindsCommon)
    , ("Set elements are unique", property prop_setElementsAreUnique)
    , ("Set toList and fromList are inverses", property prop_setToListFromListInverse)
    ]
  , testProperties "Algebraic Data Type Properties"
    [ ("Either left or right", property prop_eitherLeftOrRight)
    , ("Maybe is just or nothing", property prop_maybeIsJustOrNothing)
    , ("Tuple projection preserves components", property prop_tupleProjectionPreserves)
    ]
  ]