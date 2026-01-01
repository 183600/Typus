{-# LANGUAGE CPP #-}

module Test.Unit.BasicPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set

import Utils (trim, splitBy, removeLineComments)
import SourceLocation (SourcePos(..), SourceSpan(..), posLine, posColumn, posOffset, spanStart, spanEnd)
import Parser (FileDirectives(..), BlockDirectives(..))
import TestSupport.Arbitrary ()

-- Property 1: trim is idempotent
prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

-- Property 2: splitBy preserves L.length relationship
prop_splitBy_length :: Char -> String -> Property
prop_splitBy_length delim s =
  let parts = splitBy delim s
      delimCount = L.length (L.filter (== delim) s)
  in L.length parts === delimCount + 1

-- Property 3: SourcePos ordering is consistent
prop_sourcepos_ordering :: Property
prop_sourcepos_ordering = forAll genValidPos $ \(l1, c1, o1, l2, c2, o2) ->
  let pos1 = SourcePos l1 c1 o1
      pos2 = SourcePos l2 c2 o2
  in (o1 < o2) === (posOffset pos1 < posOffset pos2)
  where
    genValidPos = do
      l1 <- choose (1, 100)
      c1 <- choose (1, 100)
      o1 <- choose (0, 1000)
      l2 <- choose (1, 100)
      c2 <- choose (1, 100)
      o2 <- choose (0, 1000)
      return (l1, c1, o1, l2, c2, o2)

-- Property 4: SourceSpan is well-formed
prop_sourcespan_wellformed :: SourceSpan -> Property
prop_sourcespan_wellformed span =
  let start = spanStart span
      end = spanEnd span
  in conjoin
    [ posLine start > 0
    , posColumn start > 0
    , posOffset start >= 0
    , posLine end > 0
    , posColumn end > 0
    , posOffset end >= 0
    , posOffset start <= posOffset end
    ]

-- Property 5: Map insert L.and lookup consistency
prop_map_insert_lookup :: String -> Int -> Property
prop_map_insert_lookup key value =
  let m = Map.insert key value Map.empty
  in Map.lookup key m === Just value

-- Property 6: Set insert L.and member consistency
prop_set_insert_member :: Int -> Property
prop_set_insert_member value =
  let s = Set.insert value Set.empty
  in property $ Set.member value s

-- Property 7: removeLineComments preserves structure when no comments present
prop_removeLineComments_preserves :: String -> Property
prop_removeLineComments_preserves s =
  not ("//" `elem` [take 2 $ drop i s | i <- [0..max 0 (L.length s - 2)]]) ==>
  let result = removeLineComments s
      expected = if null s then "" else unlines (lines s)
  in result === expected

-- Property 8: FileDirectives equality works correctly
prop_file_directives_equality_works :: Property
prop_file_directives_equality_works =
  let fd1 = FileDirectives Nothing Nothing Nothing
      fd2 = FileDirectives Nothing Nothing Nothing
  in fd1 === fd2

-- Property 9: trim removes only whitespace
prop_trim_only_whitespace :: String -> Property
prop_trim_only_whitespace s =
  let trimmed = trim s
      isWhitespace c = c `elem` " \t\n\r"
  in L.all (not . isWhitespace) (take 1 trimmed ++ drop (L.length trimmed - 1) trimmed) === True

-- Property 10: splitBy L.and L.concat are inverse operations
prop_splitBy_concat_inverse :: Char -> [String] -> Property
prop_splitBy_concat_inverse delim parts =
  not (null parts) && L.all (L.notElem delim) parts ==>
  let joined = L.concat $ L.map (++ [delim]) (init parts) ++ [last parts]
      split = splitBy delim joined
  in split === parts

tests :: TestTree
tests = testGroup "Basic Properties QuickCheck Tests"
  [ fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy preserves L.length relationship" prop_splitBy_length
  , fastProperty "SourcePos ordering is consistent" prop_sourcepos_ordering
  , fastProperty "SourceSpan is well-formed" prop_sourcespan_wellformed
  , fastProperty "Map insert L.and lookup are consistent" prop_map_insert_lookup
  , fastProperty "Set insert L.and member are consistent" prop_set_insert_member
  , fastProperty "removeLineComments preserves non-comment lines" prop_removeLineComments_preserves
  , fastProperty "FileDirectives equality works correctly" prop_file_directives_equality_works
  , fastProperty "trim removes only whitespace from ends" prop_trim_only_whitespace
  , fastProperty "splitBy L.and L.concat are inverse operations" prop_splitBy_concat_inverse
  ]
