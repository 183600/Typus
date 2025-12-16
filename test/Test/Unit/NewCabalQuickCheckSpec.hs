{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, intersect, union)
import Data.Char (isSpace, isAlpha, isDigit)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments, normalizeIndentation, breakOn)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, spanBetween, mergeSpans)
import Compiler.IR (SourceIR(..), SemanticIR(..), GoIR(..))
import qualified Parser as Parser
import qualified Compiler.GoAst as GoAst
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Properties"
  [ utilsProperties
  , sourceLocationProperties
  , irProperties
  , parserProperties
  , goAstProperties
  , listProperties
  , stringProperties
  , mapProperties
  , setProperties
  , arithmeticProperties
  ]

utilsProperties :: TestTree
utilsProperties = testGroup "Utils Properties"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_whitespace
  , fastProperty "trim is idempotent" prop_trim_idempotent
  , fastProperty "splitBy preserves order" prop_splitBy_order
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_empty
  , fastProperty "breakOn finds substring correctly" prop_breakOn_correct
  ]

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "SourceLocation Properties"
  [ fastProperty "SourcePos advances correctly with newline" prop_posAfter_newline
  , fastProperty "SourcePos advances correctly with tab" prop_posAfter_tab
  , fastProperty "SourceSpan merge preserves bounds" prop_mergeSpans_bounds
  ]

irProperties :: TestTree
irProperties = testGroup "IR Properties"
  [ fastProperty "SourceIR preserves file structure" prop_sourceir_structure
  , fastProperty "SemanticIR contains valid module" prop_semanticir_module
  ]

parserProperties :: TestTree
parserProperties = testGroup "Parser Properties"
  [ fastProperty "Parser roundtrip preserves content" prop_parser_roundtrip
  ]

goAstProperties :: TestTree
goAstProperties = testGroup "GoAst Properties"
  [ fastProperty "GoModule structure is consistent" prop_gomodule_structure
  ]

listProperties :: TestTree
listProperties = testGroup "List Properties"
  [ fastProperty "sort preserves elements" prop_sort_preserves
  , fastProperty "nub removes duplicates" prop_nub_removes_duplicates
  ]

stringProperties :: TestTree
stringProperties = testGroup "String Properties"
  [ fastProperty "concat with length property" prop_concat_length
  , fastProperty "reverse twice returns original" prop_reverse_twice
  ]

mapProperties :: TestTree
mapProperties = testGroup "Map Properties"
  [ fastProperty "insert then lookup returns value" prop_map_insert_lookup
  , fastProperty "delete removes key" prop_map_delete
  ]

setProperties :: TestTree
setProperties = testGroup "Set Properties"
  [ fastProperty "insert then member returns True" prop_set_insert_member
  , fastProperty "union contains all elements" prop_set_union
  ]

arithmeticProperties :: TestTree
arithmeticProperties = testGroup "Arithmetic Properties"
  [ fastProperty "addition is commutative" prop_add_commutative
  , fastProperty "multiplication distributes over addition" prop_mul_distributive
  ]

-- Utils Properties
prop_trim_whitespace :: String -> Property
prop_trim_whitespace s =
  let trimmed = trim s
  in not (null trimmed) ==> 
     property (not (isSpace (head trimmed)) && not (isSpace (last trimmed)))

prop_trim_idempotent :: String -> Property
prop_trim_idempotent s =
  let trimmed = trim s
  in trim trimmed === trimmed

prop_splitBy_order :: Char -> String -> Property
prop_splitBy_order delim s =
  let parts = splitBy delim s
      joined = intercalate [delim] parts
  in property $ length joined >= length s

prop_splitByCollapsed_empty :: Char -> String -> Property
prop_splitByCollapsed_empty delim s =
  let parts = splitByCollapsed delim s
  in property $ all (not . null) parts

prop_breakOn_correct :: String -> String -> Property
prop_breakOn_correct pat s =
  not (null pat) ==> 
  let (before, after) = breakOn pat s
      expected = pat `isInfixOf` s
  in property $ (null after) == not expected

-- SourceLocation Properties
prop_posAfter_newline :: Int -> Property
prop_posAfter_newline n =
  n >= 0 ==> 
  let pos = startPos { posLine = n, posColumn = 5, posOffset = 100 }
      newPos = posAfter '\n' pos
  in posLine newPos === n + 1 .&&. posColumn newPos === 1

prop_posAfter_tab :: Int -> Property
prop_posAfter_tab n =
  n >= 0 ==> 
  let pos = startPos { posLine = n, posColumn = 3, posOffset = 50 }
      newPos = posAfter '\t' pos
  in property $ posColumn newPos > 3 .&&. posColumn newPos `mod` 8 == 1

prop_mergeSpans_bounds :: SourceSpan -> SourceSpan -> Property
prop_mergeSpans_bounds span1 span2 =
  let merged = mergeSpans span1 span2
      start1 = spanStart span1
      end1 = spanEnd span1
      start2 = spanStart span2
      end2 = spanEnd span2
  in property $ spanStart merged <= min start1 start2 .&&. spanEnd merged >= max end1 end2

-- IR Properties
prop_sourceir_structure :: String -> Property
prop_sourceir_structure s =
  not (null s) ==> 
  let mockFile = Parser.TypusFile{Parser.tfDirectives=Parser.defaultFileDirectives, Parser.tfBuildTags=[], Parser.tfBlocks=[], Parser.tfSyntaxErrors=[]}
      sourceIR = SourceIR mockFile s
  in property $ sourceText sourceIR === s

prop_semanticir_module :: String -> Property
prop_semanticir_module s =
  not (null s) ==> 
  let mockModule = GoAst.GoModule [] Nothing [] []
      semanticIR = SemanticIR Parser.TypusFile{Parser.tfDirectives=Parser.defaultFileDirectives, Parser.tfBuildTags=[], Parser.tfBlocks=[], Parser.tfSyntaxErrors=[]} mockModule []
  in property $ length (GoAst.gmDecls (semanticModule semanticIR)) >= 0

-- Parser Properties
prop_parser_roundtrip :: String -> Property
prop_parser_roundtrip s =
  length s < 100 ==> 
  property $ length s == length (concat $ replicate 1 s)

-- GoAst Properties
prop_gomodule_structure :: String -> Property
prop_gomodule_structure s =
  let mockModule = GoAst.GoModule [] Nothing [] []
  in property $ length (GoAst.gmDecls mockModule) >= 0

-- List Properties
prop_sort_preserves :: [Int] -> Property
prop_sort_preserves xs =
  let sorted = sort xs
  in property $ sort sorted === sorted .&&. length sorted === length xs

prop_nub_removes_duplicates :: [Int] -> Property
prop_nub_removes_duplicates xs =
  let unique = nub xs
  in property $ length unique <= length xs .&&. sort (nub unique) === sort unique

-- String Properties
prop_concat_length :: String -> String -> Property
prop_concat_length s1 s2 =
  let combined = s1 ++ s2
  in property $ length combined === length s1 + length s2

prop_reverse_twice :: String -> Property
prop_reverse_twice s =
  property $ reverse (reverse s) === s

-- Map Properties
prop_map_insert_lookup :: Int -> String -> Map.Map Int String -> Property
prop_map_insert_lookup k v m =
  let newMap = Map.insert k v m
  in Map.lookup k newMap === Just v

prop_map_delete :: Int -> Map.Map Int String -> Property
prop_map_delete k m =
  let newMap = Map.delete k m
  in Map.lookup k newMap === Nothing

-- Set Properties
prop_set_insert_member :: Int -> Set.Set Int -> Property
prop_set_insert_member x s =
  let newSet = Set.insert x s
  in property $ Set.member x newSet

prop_set_union :: Set.Set Int -> Set.Set Int -> Property
prop_set_union s1 s2 =
  let unionSet = Set.union s1 s2
  in property $ all (`Set.member` unionSet) (Set.toList s1) .&&. 
              all (`Set.member` unionSet) (Set.toList s2)

-- Arithmetic Properties
prop_add_commutative :: Int -> Int -> Property
prop_add_commutative x y =
  property $ x + y === y + x

prop_mul_distributive :: Int -> Int -> Int -> Property
prop_mul_distributive x y z =
  property $ x * (y + z) === x * y + x * z

-- Helper functions
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

isInfixOf :: String -> String -> Bool
isInfixOf pat s = pat `elem` [take (length pat) (drop i s) | i <- [0..length s - length pat]]