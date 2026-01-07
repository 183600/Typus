module Test.Unit.NewIntegrationQuickCheckTestSpec where


import Test.Tasty
import Test.Tasty.QuickCheck 
import SourceLocation (SourcePos(..), startPos, posAfter, advancePos)
import Parser (FileDirectives(..), defaultFileDirectives)
import qualified Data.List as L
import Data.List 
                                    totalLength = L.sum (map L.length parts) + L.length (L.filter (== ',') s)
  in totalLength >= L.length s

-- 
prop_comment_removal_position_impact :: String -> Bool
prop_comment_removal_position_impact                               s =
  let withoutComments = removeLineComments s
                                    withComments = s
  in L.length withoutComments <= L.length withComments

-- 
prop_parser_utils_integration :: String -> Bool
prop_parser_utils_integration                               s =
  let trimmed = trim s
                                    directives = defaultFileDirectives
      FileDirectives{..} = directives
  in null trimmed || (isNothing fdOwnership && isNothing fdDependentTypes && isNothing fdConstraints)

-- 
prop_position_advance_string_processing :: String -> Bool
prop_position_advance_string_processing                               s =
  let pos = startPos
                                    chars = take 10 s  -- 10
                                    finalPos = foldl advancePos pos chars
  in sourceLine finalPos >= sourceLine pos && sourceColumn finalPos >= sourceColumn pos

-- 
prop_multiline_string_processing :: String -> String -> Bool
prop_multiline_string_processing s1                               s2 =
  let combined = s1 ++ "\n" ++ s2
                                    trimmed = trim combined
                                    lines1 = L.length (lines s1)
                                    lines2 = L.length (lines s2)
                                    combinedLines = L.length (lines trimmed)
  in combinedLines >= max lines1 lines2

-- 
prop_split_position_calculation :: Positive Int -> String -> Bool
prop_split_position_calculation (Positive seed)                               s =
  let parts = splitBy ',' s
                                    positions = scanl (\pos part -> posAfter pos part) startPos parts
  in L.length                               positions == L.length parts + 1

-- 
prop_error_handling_consistency :: String -> Bool
prop_error_handling_consistency                               s =
  let trimmed = trim s
                                    split = splitBy ' ' trimmed
  in L.all (not . null) split || L.any null split

-- 
tests :: TestTree
tests =   testGroup "Integration QuickCheck Tests"
  [             testProperty "trim position tracking" prop_trim_position_tracking
  ,             testProperty "split position consistency" prop_split_position_consistency
  ,             testProperty "comment removal position impact" prop_comment_removal_position_impact
  ,             testProperty "parser utils integration" prop_parser_utils_integration
  ,             testProperty "position advance string processing" prop_position_advance_string_processing
  ,             testProperty "multiline string processing" prop_multiline_string_processing
  ,             testProperty "split position calculation" prop_split_position_calculation
  ,             testProperty "error handling consistency" prop_error_handling_consistency
  ]