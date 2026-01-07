module Test.Unit.NewCommentHandlingSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample)
import Utils ()
              assertEqual "multiple lines with comments" "line1\nline2\nline3\n" 
    (removeLineComments "line1\nline2\n// comment\nline3\n// another comment")
  
  -- 
              assertEqual "end of line comments" "code line\nanother line\n"
    (removeLineComments "code line // comment\nanother line // another comment")
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


-- | 
testLineCommentInStrings :: TestTree
testLineCommentInStrings =             testCase "Line comments in strings" $ do
  -- 
              assertEqual "comment in double quotes" "\"// not a comment\"\n" 
    (removeLineComments "\"// not a comment\"\n")
  
  -- 
              assertEqual "comment in single quotes" "'// not a comment'\n"
    (removeLineComments "'// not a comment'\n")
  
  -- 
              assertEqual "escaped quotes" "\"string with \\\" // not comment\"\n"
    (removeLineComments "\"string with \\\" // not comment\"\n")
  
  -- 
  let complexString = "code line\nstring := \"// comment inside \\\" // not comment\"\n// real comment\n"
  let expected = "code line\nstring := \"// comment inside \\\" // not comment\"\n"
              assertEqual "complex string case" expected (removeLineComments complexString)

-- | 
testLineCommentEdgeCases :: TestTree
testLineCommentEdgeCases =             testCase "Line comment edge cases" $ do
  -- 
              assertEqual "empty string" "" (removeLineComments "")
  
  -- 
              assertEqual "only comments" "" (removeLineComments "// comment only\n// another comment")
  
  -- 
              assertEqual "multiple comment markers" "" (removeLineComments "/// comment\n//// another")
  
  -- 
              assertEqual "comment at start" "code\n" (removeLineComments "//comment\ncode")
  
  -- Unicode
              assertEqual "unicode comments" "code\n" (removeLineComments "code\n// ")

-- | 
testBlockCommentBasic :: TestTree
testBlockCommentBasic =             testCase "Basic block comment handling" $ do
  -- 
              assertEqual "simple block comment" "code before\n code after\n" 
    (removeComments "code before\n/* comment */\n code after")
  
  -- 
              assertEqual "multiline block comment" "before\n after\n"
    (removeComments "before\n/* line1\nline2\nline3 */\n after")
  
  -- 
              assertEqual "inline block comment" "before  after\n"
    (removeComments "before /* comment */ after")

-- | 
testBlockCommentInStrings :: TestTree
testBlockCommentInStrings =             testCase "Block comments in strings" $ do
  -- 
              assertEqual "block comment in double quotes" "\"/* not a comment */\"\n"
    (removeComments "\"/* not a comment */\"\n")
  
  -- 
              assertEqual "block comment in single quotes" "'/* not a comment */'\n"
    (removeComments "'/* not a comment */'\n")
  
  -- 
              assertEqual "escaped characters in strings" "\"string with \\\" /* not comment */\"\n"
    (removeComments "\"string with \\\" /* not comment */\"\n")

-- | 
testBlockCommentNesting :: TestTree
testBlockCommentNesting =             testCase "Block comment nesting" $ do
  -- 
  let nested = "code /* outer /* inner */ still outer */ more code"
  let expected = "code  more code"
  result <- return $ removeComments nested
  assertBool "nested block comments" (expected == result)
  
  -- 
  let multiNested = "start /* level1 /* level2 /* level3 */ back2 */ back1 */ end"
  let expected2 = "start  end"
  result2 <- return $ removeComments multiNested
  assertBool "multi-level nested comments" (expected2 == result2)

-- | 
testBlockCommentEdgeCases :: TestTree
testBlockCommentEdgeCases =             testCase "Block comment edge cases" $ do
  -- 
              assertEqual "empty block comment" "code code" (removeComments "code /**/ code")
  
  -- 
              assertEqual "only block comment" "" (removeComments "/* only comment */")
  
  -- 
  let unmatchedOpen = "code /* comment\nmore code"
  result <- return $ removeComments unmatchedOpen
  assertBool "unmatched opening" ("code \nmore code" == result)
  
  -- 
  let unmatchedClose = "code comment */ more code"
  result2 <- return $ removeComments unmatchedClose
              assertEqual "unmatched closing" "code comment */ more code" result2

-- | 
testMixedComments :: TestTree
testMixedComments =             testCase "Mixed comment types" $ do
  -- 
  let mixed = "code // line comment\n/* block comment */\nmore code // another line"
  let expected = "code \n \nmore code "
  result <- return $ removeComments mixed
              assertEqual "mixed comment types" expected result
  
  -- 
  let blockWithLine = "code /* // this is not a line comment */ more code"
  let expected2 = "code  more code"
  result2 <- return $ removeComments blockWithLine
              assertEqual "line comment inside block" expected2 result2
  
  -- 
  let lineWithBlock = "code // /* this is not a block comment */\nmore code"
  let expected3 = "code \nmore code"
  result3 <- return $ removeComments lineWithBlock
              assertEqual "block comment inside line" expected3 result3

-- | QuickCheck 
testCommentProperties :: TestTree
testCommentProperties = testGroup "Comment Properties"
  [             testProperty "removeLineComments removes L.all line comments" $ \str ->
      let result = removeLineComments str
                                        hasLineComment = "//" `L.isInfixOf` result
      in not hasLineComment
      
  ,             testProperty "removeComments removes L.all block comments" $ \str ->
      let result = removeComments str
                                        hasBlockComment = "/*" `L.isInfixOf` result && "*/" `L.isInfixOf` result
      in not hasBlockComment
      
  ,             testProperty "removeLineComments preserves string literals" $ \str ->
      let simpleString = "\"// not a comment\""
                                        result = removeLineComments simpleString
      in                               result === simpleString
      
  ,             testProperty "removeComments preserves string literals" $ \str ->
      let simpleString = "\"/* not a comment */\""
                                        result = removeComments simpleString
      in                               result === simpleString
  ]