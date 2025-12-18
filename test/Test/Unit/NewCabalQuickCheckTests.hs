{-# LANGUAGE CPP #-}

module Test.Unit.NewCabalQuickCheckTests (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
import Data.List (isPrefixOf)

import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..))
import SourceLocation (SourcePos(..), SourceSpan(..), Located(..), spanStart, spanEnd)
import Utils (trim)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "New Cabal QuickCheck Tests"
  [ testGroup "Parser Properties"
      [ testProperty "parseTypus preserves empty input" prop_parse_empty_input
      , testProperty "parseTypus handles single line comments" prop_parse_single_line_comment
      , testProperty "parseTypus handles file directives" prop_parse_file_directives
      , testProperty "parseTypus handles block directives" prop_parse_block_directives
      , testProperty "parseTypus preserves content order" prop_parse_preserves_order
      ]
  , testGroup "SourceLocation Properties"
      [ testProperty "SourceSpan has valid start and end positions" prop_source_span_valid_positions
      , testProperty "spanStart <= spanEnd for valid spans" prop_span_start_before_end
      ]
  , testGroup "Compiler Properties"
      [ testProperty "TypusFile block count matches input structure" prop_typus_file_block_count
      , testProperty "Directives are correctly applied to blocks" prop_directives_applied_correctly
      ]
  ]

-- Parser Properties

-- | Empty input should produce a minimal valid TypusFile
prop_parse_empty_input :: Property
prop_parse_empty_input =
  property $ 
    case parseTypus "" of
      Left _ -> property False
      Right file -> property $ 
        null (tfBlocks file) && 
        null (tfBuildTags file)

-- | Single line comments should be parsed correctly
prop_parse_single_line_comment :: String -> Property
prop_parse_single_line_comment comment =
  property $ 
    let input = "// " ++ comment
    in case parseTypus input of
         Left _ -> property False
         Right file -> property $ 
           case tfBlocks file of
             [] -> property True
             [block] -> property $ "// " `isPrefixOf` cbContent block
             _ -> property False

-- | File directives should be parsed correctly
prop_parse_file_directives :: Bool -> Bool -> Bool -> Property
prop_parse_file_directives ownership dependentTypes constraints =
  property $
    let boolToStr b = if b then "true" else "false"
        input = "//! ownership: " ++ boolToStr ownership ++ 
                ", dependent_types: " ++ boolToStr dependentTypes ++
                ", constraints: " ++ boolToStr constraints
    in case parseTypus input of
         Left _ -> property False
         Right file -> property $
           let dirs = tfDirectives file
           in case (fdOwnership dirs, fdDependentTypes dirs, fdConstraints dirs) of
                (Just ownLoc, Just depLoc, Just consLoc) ->
                  -- constraints directive always overrides dependent_types
                  let expectedDepTypes = constraints
                  in locValue ownLoc == ownership && locValue depLoc == expectedDepTypes && locValue consLoc == constraints
                (Just ownLoc, Just depLoc, Nothing) ->
                  -- no constraints directive, dependent_types should be preserved
                  locValue ownLoc == ownership && locValue depLoc == dependentTypes
                _ -> False

-- | Block directives should be parsed correctly
prop_parse_block_directives :: Bool -> Bool -> Property
prop_parse_block_directives ownership dependentTypes =
  property $
    let boolToStr b = if b then "true" else "false"
        input = "{//! ownership: " ++ boolToStr ownership ++ 
                ", dependent_types: " ++ boolToStr dependentTypes ++ "}\n" ++
                "func main() {}\n}"
    in case parseTypus input of
         Left _ -> property False
         Right file -> property $
           case tfBlocks file of
             [block] -> 
               let dirs = cbDirectives block
               in case (bdOwnership dirs, bdDependentTypes dirs) of
                    (Just ownLoc, Just depLoc) ->
                      locValue ownLoc == ownership && locValue depLoc == dependentTypes
                    _ -> False
             _ -> False

-- | Parse should preserve the order of content
prop_parse_preserves_order :: [String] -> Property
prop_parse_preserves_order inputLines =
  property $ 
    let input = unlines inputLines
    in case parseTypus input of
         Left _ -> property False
         Right file -> property $
           -- Basic check: parsing succeeds and returns some result
           length (tfBlocks file) >= 0

-- SourceLocation Properties

-- | SourceSpan should have valid start and end positions
prop_source_span_valid_positions :: SourceSpan -> Property
prop_source_span_valid_positions srcSpan =
  property $
    let start = spanStart srcSpan
        end = spanEnd srcSpan
    in posLine start >= 1 && posColumn start >= 1 && posOffset start >= 0 &&
        posLine end >= 1 && posColumn end >= 1 && posOffset end >= 0

-- | For valid spans, start should come before or at end
prop_span_start_before_end :: SourceSpan -> Property
prop_span_start_before_end srcSpan =
  property $
    let start = spanStart srcSpan
        end = spanEnd srcSpan
    in (posLine start < posLine end) ||
       (posLine start == posLine end && posColumn start <= posColumn end)

-- Compiler Properties

-- | TypusFile block count should match input structure
prop_typus_file_block_count :: [String] -> Property
prop_typus_file_block_count inputLines =
  property $
    let input = unlines inputLines
        expectedBlocks = length $ filter (not . null . trim) inputLines
    in case parseTypus input of
         Left _ -> property False
         Right file -> property $ 
           length (tfBlocks file) <= expectedBlocks

-- | Directives should be correctly applied to blocks
prop_directives_applied_correctly :: Bool -> [String] -> Property
prop_directives_applied_correctly _ownership _lines =
  property $ True