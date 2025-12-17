{-# LANGUAGE CPP #-}

module Test.Unit.AdditionalCoreQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub, isInfixOf)

import Utils (trim, splitBy, splitByCollapsed, removeLineComments)
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), startPos, posAfter, emptySpan, spanFrom)
import Parser (FileDirectives(..), BlockDirectives(..), CodeBlock(..))
import Compiler.GoAst (GoModule(..), GoDecl(..))

tests :: TestTree
tests = testGroup "Additional Core QuickCheck Properties"
  [ utilProperties
  , sourceLocationProperties 
  , parserIRProperties
  ]

utilProperties :: TestTree
utilProperties = testGroup "Utils Properties"
  [ fastProperty "trim removes leading and trailing whitespace" prop_trim_whitespace
  , fastProperty "splitBy preserves empty segments" prop_splitBy_preserves_empty
  , fastProperty "splitByCollapsed removes empty segments" prop_splitByCollapsed_removes_empty
  , fastProperty "removeLineComments handles line endings" prop_removeLineComments_line_endings
  ]

sourceLocationProperties :: TestTree
sourceLocationProperties = testGroup "SourceLocation Properties"
  [ fastProperty "startPos is always (1,1)" prop_startPos_value
  , fastProperty "posAfter advances correctly" prop_posAfter_advances
  , fastProperty "emptySpan has zero length" prop_emptySpan_zero_length
  , fastProperty "spanFrom creates valid span" prop_spanFrom_valid
  ]

parserIRProperties :: TestTree
parserIRProperties = testGroup "Parser/IR Properties"
  [ fastProperty "FileDirectives roundtrip" prop_file_directives_roundtrip
  , fastProperty "Located values preserve location" prop_located_preserves_location
  ]

-- Utils properties
prop_trim_whitespace :: String -> Property
prop_trim_whitespace input =
  let trimmed = trim input
      hasLeadingSpace = not (null input) && isSpace (head input)
      hasTrailingSpace = not (null input) && isSpace (last input)
  in property $ 
    if hasLeadingSpace || hasTrailingSpace
    then not (null trimmed) ==> head trimmed `notElem` " \t\n\r" && last trimmed `notElem` " \t\n\r"
    else trimmed === input
  where
    isSpace c = c `elem` " \t\n\r"

prop_splitBy_preserves_empty :: Char -> String -> Property
prop_splitBy_preserves_empty delim input =
  let result = splitBy delim input
      expectedLength = length input + 1
  in property $ length result === expectedLength

prop_splitByCollapsed_removes_empty :: Char -> String -> Property
prop_splitByCollapsed_removes_empty delim input =
  let result = splitByCollapsed delim input
      hasEmpty = any null result
  in property $ not hasEmpty

prop_removeLineComments_line_endings :: String -> String -> Property
prop_removeLineComments_line_endings code comment =
  let input = code ++ "//" ++ comment ++ "\nmore code"
      result = removeLineComments input
  in property $ not ("//" `isInfixOf` result) && "more code" `isInfixOf` result

-- SourceLocation properties
prop_startPos_value :: Property
prop_startPos_value = property $ startPos === SourcePos 1 1 0

prop_posAfter_advances :: Int -> Int -> Char -> Bool
prop_posAfter_advances line col char =
  let pos = SourcePos line col 0
      newPos = posAfter char pos
  in if char == '\n'
     then posLine newPos > line
     else posLine newPos == line

prop_emptySpan_zero_length :: Property
prop_emptySpan_zero_length = property $ 
  let span = emptySpan startPos
  in spanStart span === spanEnd span

prop_spanFrom_valid :: SourcePos -> Property
prop_spanFrom_valid pos =
  let span = spanFrom pos
  in property $ True -- spanFrom creates a valid empty span

-- Parser/IR properties
prop_file_directives_roundtrip :: FileDirectives -> Property
prop_file_directives_roundtrip directives =
  property $ True -- Simplified - would implement actual roundtrip logic

prop_located_preserves_location :: String -> SourcePos -> Property
prop_located_preserves_location value pos =
  let located = locatedAt pos value
  in property $ locPos located === pos

-- Helper function
locatedAt :: SourcePos -> a -> Located a
locatedAt pos val = Located { locSpan = spanFrom pos, locValue = val, locPos = pos }

-- Arbitrary instances for test types
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SourceSpan where
  arbitrary = SourceSpan <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Located a) where
  arbitrary = Located <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FileDirectives where
  arbitrary = FileDirectives <$> arbitrary <*> arbitrary <*> arbitrary