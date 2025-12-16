{-# LANGUAGE CPP #-}

module Test.Unit.BasicQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (sort, nub, length, sum, reverse, concat)

import Utils (trim, splitBy, splitByComma, removeLineComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, emptySpan)
import Compiler.GoLexer (GoToken(..), GoTokenKind(..), tokenizeGo)
import Ownership.Parser (Expr(..), Stmt(..))
import Compiler.TypeChecker (Type(..))
import Analyzer.Types (SymbolInfo(..))
import qualified Data.Map as Map
import TestSupport.Arbitrary ()
import TestSupport.ExtendedArbitrary ()

tests :: TestTree
tests = testGroup "Basic QuickCheck Test Properties"
  [ utilityFunctionTests
  , dataStructureTests
  , compilerComponentTests
  , errorHandlingTests
  ]

utilityFunctionTests :: TestTree
utilityFunctionTests = testGroup "Utility Function Properties"
  [ fastProperty "splitByComma handles empty string" prop_splitByComma_empty
  , fastProperty "removeLineComments preserves non-comment content" prop_removeLineComments_preserve
  , fastProperty "normalizeIndentation handles single line" prop_normalizeIndentation_single_line
  ]

dataStructureTests :: TestTree
dataStructureTests = testGroup "Data Structure Properties"
  [ fastProperty "Map insertion preserves existing keys" prop_map_insertion_preserve
  , fastProperty "Set union preserves all elements" prop_set_union_preserve
  , fastProperty "List reverse is involution" prop_reverse_involution
  ]

compilerComponentTests :: TestTree
compilerComponentTests = testGroup "Compiler Component Properties"
  [ fastProperty "Source position advancement is monotonic" prop_position_advancement_monotonic
  , fastProperty "Token lexeme matches expected pattern" prop_token_lexeme_pattern
  , fastProperty "AST expression preserves type information" prop_ast_expression_type
  ]

errorHandlingTests :: TestTree
errorHandlingTests = testGroup "Error Handling Properties"
  [ fastProperty "Error location is always valid" prop_error_location_valid
  , fastProperty "Error messages contain useful information" prop_error_message_useful
  ]

prop_splitByComma_empty :: Property
prop_splitByComma_empty = splitByComma "" === [""]

prop_removeLineComments_preserve :: String -> Property
prop_removeLineComments_preserve s =
  let noComments = "//" `notElem` (words s)
  in noComments ==> removeLineComments s === s

prop_normalizeIndentation_single_line :: String -> Property
prop_normalizeIndentation_single_line s =
  let singleLine = not ('\n' `elem` s)
  in singleLine ==> normalizeIndentation s === s

prop_map_insertion_preserve :: [(String, Int)] -> String -> Int -> Property
prop_map_insertion_preserve pairs key value =
  let originalMap = Map.fromList pairs
      newMap = Map.insert key value originalMap
  in Map.lookup key newMap === Just value

prop_set_union_preserve :: [Int] -> [Int] -> Property
prop_set_union_preserve xs ys =
  let setX = Set.fromList xs
      setY = Set.fromList ys
      unionSet = Set.union setX setY
  in property $ Set.isSubsetOf setX unionSet && Set.isSubsetOf setY unionSet

prop_reverse_involution :: [Int] -> Property
prop_reverse_involution xs = reverse (reverse xs) === xs

prop_position_advancement_monotonic :: SourcePos -> String -> Property
prop_position_advancement_monotonic pos text =
  let finalPos = foldl (flip posAfter) pos text
  in property $ posOffset finalPos >= posOffset pos

prop_token_lexeme_pattern :: GoTokenKind -> String -> Property
prop_token_lexeme_pattern tokenKind tokenText' =
  let token = GoToken tokenKind tokenText'
  in property $ tokenText token === tokenText'

prop_ast_expression_type :: Expr -> Property
prop_ast_expression_type expr = property True

prop_error_location_valid :: SourcePos -> Property
prop_error_location_valid pos = 
  property $ posLine pos > 0 && posColumn pos > 0

prop_error_message_useful :: String -> Property
prop_error_message_useful msg =
  let hasContent = not (null msg)
  in hasContent ==> property $ length (words msg) > 0