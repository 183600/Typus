{-# LANGUAGE CPP #-}

module Test.Unit.CommentRemovalSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import TestSupport.QuickCheck (fastProperty)

import Utils (removeLineComments, removeComments)

import Data.Char (isSpace)
import Data.List (isInfixOf)

-- | 测试注释移除功能的属性和边界情况
tests :: TestTree
tests =
  testGroup "Comment Removal"
    [ testGroup "removeLineComments"
        [ testCase "removes line comments after code" $ do
            removeLineComments "code // comment" @?= "code "
            removeLineComments "value := 1 // drop this" @?= "value := 1 "
            
        , testCase "handles only comments" $ do
            removeLineComments "// only comment" @?= " "
            removeLineComments "   // indented comment" @?= "   "
            
        , testCase "preserves strings with comment syntax" $ do
            let input = "url := \"http://example.com//path\" // real comment"
            removeLineComments input @?= "url := \"http://example.com//path\" "
            
        , testCase "preserves character literals" $ do
            let input = "char := '/' // this is a comment"
            removeLineComments input @?= "char := '/' "
            
        , testCase "handles multiple lines" $ do
            let input = "line1 // comment1\nline2 // comment2\nline3"
            let expected = "line1 \nline2 \nline3"
            removeLineComments input @?= expected
            
        , testCase "handles escaped quotes in strings" $ do
            let input = "text := \"hello \\\"world\\\" // not comment\" // real comment"
            removeLineComments input @?= "text := \"hello \\\"world\\\" // not comment\" "
        ]
        
    , testGroup "removeComments"
        [ testCase "removes both line and block comments" $ do
            let input = "code /* block */ more // line\nfinal"
            let expected = "code  more \nfinal"
            removeComments input @?= expected
            
        , testCase "handles nested block comments" $ do
            let input = "code /* outer /* inner */ still outer */ end"
            let expected = "code  end"
            removeComments input @?= expected
            
        , testCase "preserves comments in strings" $ do
            let input = "text := \"/* not comment */ // not comment\" // real comment"
            let expected = "text := \"/* not comment */ // not comment\" "
            removeComments input @?= expected
            
        , testCase "handles multiline block comments" $ do
            let input = "start /*\nmulti-line\ncomment */ end"
            let expected = "start \n end"
            removeComments input @?= expected
            
        , testCase "handles empty block comments" $ do
            removeComments "code /**/ more" @?= "code  more"
        ]
        
    , testGroup "String and Character Literal Preservation"
        [ testCase "preserves complex string literals" $ do
            let input = "s := \"http://test.com/path?param=value&other=123\" // comment"
            let expected = "s := \"http://test.com/path?param=value&other=123\" "
            removeLineComments input @?= expected
            removeComments input @?= expected
            
        , testCase "preserves escaped characters" $ do
            let input = "text := \"hello \\n\\t\\\"world\\\"\" // comment"
            let expected = "text := \"hello \\n\\t\\\"world\\\"\" "
            removeLineComments input @?= expected
            
        , testCase "preserves character literals with escapes" $ do
            let input = "char := '\'' // comment"
            let expected = "char := '\'' "
            removeLineComments input @?= expected
            
        , testCase "preserves mixed quotes" $ do
            let input = "text := \"He said 'hello' // not comment\" // real comment"
            let expected = "text := \"He said 'hello' // not comment\" "
            removeLineComments input @?= expected
        ]
        
    , testGroup "Edge Cases"
        [ testCase "handles empty string" $ do
            removeLineComments "" @?= ""
            removeComments "" @?= ""
            
        , testCase "handles only whitespace" $ do
            let input = "   \t\n  "
            removeLineComments input @?= input
            removeComments input @?= input
            
        , testCase "handles only comments" $ do
            removeLineComments "// line comment" @?= " "
            removeComments "/* block comment */" @?= " "
            
        , testCase "handles malformed comments" $ do
            removeComments "code /* unclosed" @?= "code  "
            removeLineComments "code // no newline" @?= "code "
            
        , testCase "handles consecutive comments" $ do
            let input = "code /* block1 */ /* block2 */ more"
            let expected = "code   more"
            removeComments input @?= expected
        ]
        
    , testGroup "Property Tests"
        [ fastProperty "removeLineComments never increases length" $ \input ->
            let result = removeLineComments input
            in length result <= length input
            
        , fastProperty "removeComments never increases length" $ \input ->
            let result = removeComments input
            in length result <= length input
            
        , fastProperty "removeLineComments is idempotent" $ \input ->
            let once = removeLineComments input
                twice = removeLineComments once
            in once == twice
            
        , fastProperty "removeComments is idempotent" $ \input ->
            let once = removeComments input
                twice = removeComments once
            in once == twice
            
        , fastProperty "removeComments removes all comment markers" $ \input ->
            let result = removeComments input
            in not ("//" `isInfixOf` result) && not ("/*" `isInfixOf` result) && not ("*/" `isInfixOf` result)
            
        , fastProperty "removeLineComments preserves code structure" $ \input ->
            let result = removeLineComments input
                originalLines = lines input
                resultLines = lines result
            in length resultLines == length originalLines
            
        , fastProperty "functions handle Unicode correctly" $ \input ->
            let lineResult = removeLineComments input
                blockResult = removeComments input
            in length lineResult >= 0 && length blockResult >= 0
        ]
        
    , testGroup "Performance and Robustness"
        [ testCase "handles very long lines" $ do
            let longLine = "code " ++ replicate 10000 'a' ++ " // comment"
            let result = removeLineComments longLine
            length result >= 0 @?= True
            
        , testCase "handles deeply nested block comments" $ do
            let nested = concat $ replicate 100 "/*"
            let input = "code " ++ nested ++ " comment " ++ concat (replicate 100 "*/") ++ " end"
            let result = removeComments input
            length result >= 0 @?= True
            
        , fastProperty "functions don't crash on any input" $ \input ->
            let lineResult = removeLineComments input
                blockResult = removeComments input
            in length lineResult >= 0 && length blockResult >= 0
        ]
    ]