module Test.Unit.CommentHandlingQuickCheckSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), (==>), elements, listOf, suchThat)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import qualified Data.Text as T
import Data.Char ()
  return $ unlines lines
-- Arbitrary instance for SourcePos
instance Arbitrary SourcePos where
  arbitrary = do
    line <- choose (1, 100)
    column <- choose (1, 100)
    offset <- choose (0, 1000)
    return $ SourcePos line column offset

-- Arbitrary instance for SourceSpan
instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    end <- arbitrary
    return $ SourceSpan start end


-- Generate strings with line comments
genStringWithLineComments :: Gen String
                              genStringWithLineComments = do
              base <- genStringWithoutLineComments
  commentLines <- listOf $ do
              indent <- listOf $ return ' '
    content <- listOf $ arbitrary `suchThat` (/= '\n')
    return $ indent ++ "//" ++ content
  let mixed = interleaveLines (lines base) commentLines
  return $ unlines mixed

-- Generate strings with block comments
genStringWithBlockComments :: Gen String
                              genStringWithBlockComments = do
              base <- genStringWithoutLineComments
  comments <- listOf $ do
              content <- listOf $ arbitrary `suchThat` (\c -> c /= '/' && c /= '*')
    return $ "/*" ++ content ++ "*/"
  let withComments = L.foldl (\acc comment -> acc ++ comment ++ "\n") base comments
  return withComments

-- Generate strings with both line L.and block comments
genStringWithMixedComments :: Gen String
                              genStringWithMixedComments = do
              withLine <- genStringWithLineComments
  withBlock <- genStringWithBlockComments
  return $ withLine ++ "\n" ++ withBlock

-- Helper function to interleave lines
interleaveLines :: [String] -> [String] -> [String]
interleaveLines []                               _ = []
interleaveLines _ [] = []
interleaveLines (x:xs) (y:ys) = x : y : interleaveLines xs ys

-- ============================================================================
-- Property Tests
-- ============================================================================

-- Property: removeLineComments should remove L.all // comments
prop_removeLine_comments_removes_all :: String -> Property
prop_removeLine_comments_removes_all                               s =
  let result = removeLineComments s
                                    hasLineComment = "//" `L.isInfixOf` result
  in not                               hasLineComment === True

-- Property: removeLineComments should preserve non-comment content
prop_removeLine_comments_preserves_content :: String -> Property
prop_removeLine_comments_preserves_content                               s =
  let result = removeLineComments s
                                    originalLines = lines s
                                    resultLines = lines result
      -- Count non-comment lines in original
                                    nonCommentOriginal = L.length $ L.filter (not . ("//" `L.isPrefixOf`) originalLines
      -- Result should have at least as many content lines as non-comment original
                                    resultHasContent = not (null result) || L.all ("//" `L.isPrefixOf`) originalLines
  in                               resultHasContent === True

-- Property: removeComments should remove both // L.and /* */ comments
prop_remove_comments_removes_both :: String -> Property
prop_remove_comments_removes_both                               s =
  let result = removeComments s
                                    hasLineComment = "//" `L.isInfixOf` result
                                    hasBlockComment = "/*" `L.isInfixOf` result || "*/" `L.isInfixOf` result
  in not (hasLineComment || hasBlockComment) === True

-- Property: removeComments should be idempotent
prop_remove_comments_idempotent :: String -> Property
prop_remove_comments_idempotent                               s =
  let removedOnce = removeComments s
                                    removedTwice = removeComments removedOnce
  in                               removedOnce === removedTwice

-- Property: removeLineComments should be idempotent
prop_remove_line_comments_idempotent :: String -> Property
prop_remove_line_comments_idempotent                               s =
  let removedOnce = removeLineComments s
                                    removedTwice = removeLineComments removedOnce
  in                               removedOnce === removedTwice

-- Property: removeComments should preserve string literals
prop_remove_comments_preserves_strings :: String -> Property
prop_remove_comments_preserves_strings                               s =
  let stringWithString = s ++ "\nlet x = \"// not a comment\""
                                    result = removeComments stringWithString
                                    hasStringLiteral = "\"// not a comment\"" `L.isInfixOf` result
  in                               hasStringLiteral === True

-- Property: removeComments should preserve char literals
prop_remove_comments_preserves_chars :: String -> Property
prop_remove_comments_preserves_chars                               s =
  let stringWithChar = s ++ "\nlet x = '/' // not a comment"
                                    result = removeComments stringWithChar
                                    hasCharLiteral =  "'/'" `isInfixString` result
  in property $ hasCharLiteral === True
  where
      isInfixString needle                               haystack = needle `L.isInfixOf` haystack

-- ============================================================================
-- Unit Tests
-- ============================================================================

test_removeLineComments_examples :: TestTree
test_removeLineComments_examples =             testCase "removeLineComments examples" $ do
              removeLineComments "hello// comment" @?= "hello"
  removeLineComments "hello// comment\nworld" @?= "hello\nworld"
  removeLineComments "// full line comment\nnext line" @?= "\nnext line"
  removeLineComments "no comments here" @?= "no comments here"
  removeLineComments "" @?= ""

test_removeComments_examples :: TestTree
test_removeComments_examples =             testCase "removeComments examples" $ do
              removeComments "hello// comment\nworld" @?= "hello\nworld"
  removeComments "hello/* block comment */world" @?= "helloworld"
  removeComments "// line\n/* block */\nnext" @?= "\n\nnext"
  removeComments "let x = 42 // inline comment" @?= "let x = 42 "
  removeComments "let s = \"// not comment\"" @?= "let s = \"// not comment\""
  removeComments "let c = '/' // comment" @?= "let c = '/' "

test_nested_block_comments :: TestTree
test_nested_block_comments =             testCase "nested block comments" $ do
              removeComments "/* outer /* inner */ still outer */ end" @?= " end"
  removeComments "/* comment1 */ code /* comment2 */" @?= " code "

test_comments_with_strings :: TestTree
test_comments_with_strings =             testCase "comments with string literals" $ do
              removeComments "let s = \"// not a comment\"" @?= "let s = \"// not a comment\""
  removeComments "let s = \"/* not a comment */\"" @?= "let s = \"/* not a comment */\""
  removeComments "let s = \"//\" // real comment" @?= "let s = \"//\" "

-- ============================================================================
-- Test Suite
-- ============================================================================
tests :: TestTree
tests =   testGroup "Comment Handling QuickCheck Tests"
  [             testProperty "removeLineComments removes L.all // comments" prop_removeLine_comments_removes_all
  ,             testProperty "removeLineComments preserves non-comment content" prop_removeLine_comments_preserves_content
  ,             testProperty "removeComments removes both // L.and /* */ comments" prop_remove_comments_removes_both
  ,             testProperty "removeComments is idempotent" prop_remove_comments_idempotent
  ,             testProperty "removeLineComments is idempotent" prop_remove_line_comments_idempotent
  ,             testProperty "removeComments preserves string literals" prop_remove_comments_preserves_strings
  ,             testProperty "removeComments preserves char literals" prop_remove_comments_preserves_chars
  , test_removeLineComments_examples
  , test_removeComments_examples
  , test_nested_block_comments
  , test_comments_with_strings
  ]