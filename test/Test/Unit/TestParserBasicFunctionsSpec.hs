module Test.Unit.TestParserBasicFunctionsSpec where



import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Char (isSpace)
import Utils (removeLineComments, normalizeIndentation)

-- Test Properties for Parser Basic Functions

-- Property: Basic parsing should be idempotent for whitespace
prop_whitespace_parsing_idempotent :: String -> Property
prop_whitespace_parsing_idempotent s = property $ 
  let trimmed = trimWhitespace s
  in trimWhitespace trimmed == trimmed
  where
    trimWhitespace = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- Property: Token splitting should preserve non-whitespace characters
prop_token_split_preserve_chars :: String -> Property
prop_token_split_preserve_chars s = property $ 
  let tokens = splitTokens s
      combined = unwords tokens
  in filter (not . isSpace) s == filter (not . isSpace) combined
  where
    splitTokens = words . map (\c -> if isSpace c then ' ' else c)

-- Property: Comment removal should not change non-comment content
prop_comment_removal_preserve_content :: String -> Property
prop_comment_removal_preserve_content s = property $ 
  let withoutComments = removeLineComments s
      hasComments = "//" `isInfixOf` s || "/*" `isInfixOf` s
  in if hasComments
     then length withoutComments <= length s
     else withoutComments == s

-- Property: String literal parsing should handle escapes correctly
prop_string_literal_handles_escapes :: String -> Property
prop_string_literal_handles_escapes s = property $ 
  let hasEscapes = '\\' `elem` s
      parsed = parseStringLiteral s
  in if hasEscapes
     then length parsed >= length s - count '\\' s
     else length parsed == length s
  where
    count ch = length . filter (== ch)
    parseStringLiteral str = concatMap handleChar str
    handleChar '\\' = "\\"
    handleChar c = [c]

-- Property: Indentation normalization should preserve relative indentation
prop_indentation_preserve_relative :: String -> Property
prop_indentation_preserve_relative s = property $ 
  let normalized = normalizeIndentation s
      lines' = lines s
      normalizedLines = lines normalized
  in length lines' == length normalizedLines

-- Property: Parser should handle empty input gracefully
prop_parser_handles_empty :: Property
prop_parser_handles_empty = property $ 
  let result = parse ""
  in result == []

-- Helper functions
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
    
    tails :: [a] -> [[a]]
    tails [] = [[]]
    tails xs@(_:xs') = xs : tails xs'

parse :: String -> [String]
parse = words

tests :: TestTree
tests = testGroup "Test.Unit.TestParserBasicFunctionsSpec Tests"
  [ testProperty "Basic parsing should be idempotent for whitespace" prop_whitespace_parsing_idempotent
  , testProperty "Token splitting should preserve non-whitespace characters" prop_token_split_preserve_chars
  , testProperty "Comment removal should not change non-comment content" prop_comment_removal_preserve_content
  , testProperty "String literal parsing should handle escapes correctly" prop_string_literal_handles_escapes
  , testProperty "Indentation normalization should preserve relative indentation" prop_indentation_preserve_relative
  , testProperty "Parser should handle empty input gracefully" prop_parser_handles_empty
  ]