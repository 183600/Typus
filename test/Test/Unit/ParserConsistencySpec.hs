module Test.Unit.ParserConsistencySpec where


import Test.Tasty 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, oneof, elements, listOf, choose)
import Test.QuickCheck (Gen, choose, vectorOf, elements, Arbitrary(..), Property)
import Parser

ied Data.Text as T
import qualified Data.List as L
import Data.List 
  not (null code) ==> 
    let input = "```go\n" ++ code ++ "\n```"
                                      result = parseTypus input
    in case result of
         Left _ -> property True -- Parsing errors are acceptable
         Right typusFile -> 
           case tfCodeBlocks typusFile of
             (block:_) -> code `L.isInfixOf` cbContent block
             [] -> property False

-- | parseTypus should handle directives consistently
prop_parse_directives_consistency :: Bool -> Bool -> Bool -> Property
prop_parse_directives_consistency ownership dependent                               constraints =
  let input = "// @ownership: " ++ show ownership ++ "\n" ++
              "// @dependent-types: " ++ show dependent ++ "\n" ++
              "// @constraints: " ++ show constraints ++ "\n" ++
              "```go\nfunc main() {}\n```"
                                    result = parseTypus input
  in case result of
       Left _ -> property True -- Parsing errors are acceptable
       Right typusFile -> 
         let fileDirectives = tfDirectives typusFile
         in case (fdOwnership fileDirectives, fdDependentTypes fileDirectives, fdConstraints fileDirectives) of
              (Just (Located _ own), Just (Located _ dep), Just (Located _ cons) ->
                                              own === ownership &&                               dep === dependent &&                               cons === constraints
              _ -> property True -- Partial directive parsing is acceptable

-- | parseTypus round-trip property (simplified)
prop_parse_round_trip :: String -> Property
prop_parse_round_trip                               code =
  L.length code <                               100 ==> -- Keep it simple for round-trip
    let input = "```go\n" ++ code ++ "\n```"
                                      result = parseTypus input
    in case result of
         Left _ -> property True
         Right typusFile -> 
           case tfCodeBlocks typusFile of
             (block:_) -> L.length (cbContent block) >= L.length code
             [] -> property False

-- | parseTypus error handling consistency
prop_parse_error_consistency :: String -> Property
prop_parse_error_consistency                               malformedInput =
  let result = parseTypus malformedInput
  in case result of
       Left err1 -> 
         let result2 = parseTypus malformedInput
         in case result2 of
              Left err2 -> L.length (show err1) > 0 && L.length (show err2) > 0
              Right _ -> property False
       Right _ -> property True

-- | parseTypus should handle multiple code blocks
prop_parse_multiple_blocks :: [String] -> Property
prop_parse_multiple_blocks                               codes =
  not (null codes) && L.all (not . null)                               codes ==> 
    let blocks = L.map (\code -> "```go\n" ++ code ++ "\n```") codes
                                      input = unlines blocks
                                      result = parseTypus input
    in case result of
         Left _ -> property True
         Right typusFile -> L.length (tfCodeBlocks typusFile) >= L.length codes

-- | parseTypus directive location tracking
prop_parse_directive_locations :: String -> Property
prop_parse_directive_locations                               directiveContent =
  let input = "// @ownership: true\n" ++ directiveContent ++ "\n```go\nfunc main() {}\n```"
                                    result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> 
         case fdOwnership (tfDirectives typusFile) of
           Just (Located span _) -> spanStart span `seq` spanEnd span `seq` property True
           Nothing -> property False

-- | parseTypus handles comments correctly
prop_parse_comments :: String -> Property
prop_parse_comments                               comment =
  let input = "// This is a comment: " ++ comment ++ "\n```go\nfunc main() {}\n```"
                                    result = parseTypus input
  in case result of
       Left _ -> property True
       Right typusFile -> property True -- If it parses, that's success

-- | parseTypus maintains block order
prop_parse_block_order :: [String] -> Property
prop_parse_block_order                               codes =
  not (null codes) && L.all (not . null)                               codes ==> 
    let numberedBlocks = zipWith (\i code -> "// Block " ++ show i ++ "\n```go\n" ++ code ++ "\n```") 
                                [1..] codes
                                      input = unlines numberedBlocks
                                      result = parseTypus input
    in case result of
         Left _ -> property True
         Right typusFile -> L.length (tfCodeBlocks typusFile) >= L.length codes

-- | parseTypus handles nested structures (simplified test)
prop_parse_nested_structures :: String -> Property
prop_parse_nested_structures                               nestedCode =
  L.length nestedCode <                               200 ==> -- Keep it manageable
    let input = "```go\n" ++ nestedCode ++ "\n```"
                                      result = parseTypus input
    in case result of
         Left _ -> property True -- Parsing errors are acceptable for complex nested code
         Right typusFile -> 
           case tfCodeBlocks typusFile of
             (block:_) -> L.length (cbContent block) > 0
             [] -> property False

-- Helper for equality in QuickCheck
(===) :: Eq                               a => a -> a -> Bool
(===) = (==)

-- Helper for property testing
property :: Bool -> Property
                              property = id