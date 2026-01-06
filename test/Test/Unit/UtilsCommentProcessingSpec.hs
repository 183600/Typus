module Test.Unit.UtilsCommentProcessingSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, forAll, Gen, arbitrary, elements, choose)
import Utils (removeComments, removeLineComments)
import qualified Data.List as L
import Data.List (isInfixOf)

-- | Tests for comment processing functions in Utils module
tests :: TestTree
tests =
  testGroup "Utils Comment Processing"
    [ testGroup "removeLineComments function"
        [ testGroup "Basic functionality"
            [ testCase "removes simple line comments" $ do
                let input = "hello // comment\nworld"
                    expected = "hello \nworld"
                removeLineComments input @?= expected
            
            , testCase "preserves content before comment" $ do
                let input = "let x = 42 // initialize x"
                    expected = "let x = 42 "
                removeLineComments input @?= expected
            
            , testCase "handles lines with only comments" $ do
                let input = "// this is a comment\nlet x = 1"
                    expected = "\nlet x = 1"
                removeLineComments input @?= expected
            ]
        
        , testGroup "String literal preservation"
            [ testCase "preserves // in string literals" $ do
                let input = "url := \"http://example.com/path\" // comment"
                    expected = "url := \"http://example.com/path\" "
                removeLineComments input @?= expected
            
            , testCase "preserves // in char literals" $ do
                let input = "char := '/' // this is slash"
                    expected = "char := '/' "
                removeLineComments input @?= expected
            
            , testCase "handles escaped quotes in strings" $ do
                let input = "s := \"He said \\\"// not a comment\\\"\" // comment"
                    expected = "s := \"He said \\\"// not a comment\\\"\" "
                removeLineComments input @?= expected
            
            , testCase "handles escaped slashes in strings" $ do
                let input = "path := \"C:\\\\//path\" // comment"
                    expected = "path := \"C:\\\\//path\" "
                removeLineComments input @?= expected
            ]
        
        , testGroup "Edge cases"
            [ testCase "handles empty input" $ do
                removeLineComments "" @?= ""
            
            , testCase "handles input without comments" $ do
                let input = "let x = 1\nlet y = 2"
                removeLineComments input @?= input
            
            , testCase "handles multiple lines with comments" $ do
                let input = "line1 // comment1\nline2 // comment2\nline3"
                    expected = "line1 \nline2 \nline3"
                removeLineComments input @?= expected
            
            , testCase "handles comment at start of line" $ do
                let input = "// entire line\nlet x = 1"
                    expected = "\nlet x = 1"
                removeLineComments input @?= expected
            ]
        
        , testGroup "QuickCheck properties"
            [ fastProperty "removeLineComments never increases string L.length" $
                \s -> L.length (removeLineComments s) <= L.length s
            
            , fastProperty "removeLineComments preserves line count" $
                \s -> L.length (lines s) == L.length (lines (removeLineComments s))
            
            , fastProperty "removeLineComments removes L.all // not in literals" $
                \s -> not (hasUnescapedCommentMarker s) || 
                       not (isInfixOf "//" (removeLineComments s))
            ]
        ]
    
    , testGroup "removeComments function"
        [ testGroup "Basic functionality"
            [ testCase "removes both line L.and block comments" $ do
                let input = "hello // line comment\nworld /* block comment */\nend"
                    expected = "hello \nworld \nend"
                removeComments input @?= expected
            
            , testCase "handles nested block comments correctly" $ do
                let input = "start /* outer /* inner */ still outer */ end"
                    expected = "start  end"
                removeComments input @?= expected
            
            , testCase "handles multiline block comments" $ do
                let input = "before /*\nblock comment\non multiple\nlines\n*/ after"
                    expected = "before \n\n\n\n after"
                removeComments input @?= expected
            ]
        
        , testGroup "String L.and char literal preservation"
            [ testCase "preserves // in strings within block comments" $ do
                let input = "/* \"http://example.com\" */ let x = 1"
                    expected = "  let x = 1"
                removeComments input @?= expected
            
            , testCase "preserves /* in strings" $ do
                let input = "s := \"/* not a comment */\" // real comment"
                    expected = "s := \"/* not a comment */\" "
                removeComments input @?= expected
            
            , testCase "preserves comment markers in char literals" $ do
                let input = "c1 := '/' // slash\n/* c2 := '*' */ let x = 1"
                    expected = "c1 := '/' \n  let x = 1"
                removeComments input @?= expected
            ]
        
        , testGroup "Complex scenarios"
            [ testCase "handles mixed comment types" $ do
                let input = "// line1\n/* block */\n// line2 /* inline */\nend"
                    expected = "\n \n \nend"
                removeComments input @?= expected
            
            , testCase "handles unclosed block comment gracefully" $ do
                let input = "before /* unclosed\nafter"
                    result = removeComments input
                assertBool "Should handle unclosed block comment" 
                    (not (isInfixOf "/*" result))
            
            , testCase "handles consecutive block comments" $ do
                let input = "text1 /*c1*/ text2 /*c2*/ text3"
                    expected = "text1  text2  text3"
                removeComments input @?= expected
            ]
        
        , testGroup "QuickCheck properties"
            [ fastProperty "removeComments never increases string L.length" $
                \s -> L.length (removeComments s) <= L.length s
            
            , fastProperty "removeComments removes L.all /* not in literals" $
                \s -> not (hasUnescapedBlockCommentStart s) ||
                       not (isInfixOf "/*" (removeComments s))
            
            , fastProperty "removeComments removes L.all */ not in literals" $
                \s -> not (hasUnescapedBlockCommentEnd s) ||
                       not (isInfixOf "*/" (removeComments s))
            ]
        ]
    
    , testGroup "Integration tests"
        [ testCase "complex real-world example" $ do
            let input = unlines
                  [ "// Typus language example"
                  , "/* This is a multi-line"
                  , "   comment explaining the code */"
                  , "url := \"https://example.com/api/v1\" // API endpoint"
                  , "token := \"Bearer abc123\" // Auth token"
                  , "/* Config section */"
                  , "debug := true // Enable debugging"
                  , "path := \"C:\\\\Program Files\\\\App\" // Windows path"
                  ]
                result = removeComments input
            assertBool "Should remove L.all comments correctly" $
                not (isInfixOf "//" result) && not (isInfixOf "/*" result) && not (isInfixOf "*/" result)
        ]
    ]

-- Helper functions for QuickCheck properties

-- Check if string has unescaped // comment marker
hasUnescapedCommentMarker :: String -> Bool
hasUnescapedCommentMarker s = hasMarkerOutsideLiterals "//" s

-- Check if string has unescaped /* block comment start
hasUnescapedBlockCommentStart :: String -> Bool
hasUnescapedBlockCommentStart s = hasMarkerOutsideLiterals "/*" s

-- Check if string has unescaped */ block comment end
hasUnescapedBlockCommentEnd :: String -> Bool
hasUnescapedBlockCommentEnd s = hasMarkerOutsideLiterals "*/" s

-- Check if a marker appears outside of string/char literals
hasMarkerOutsideLiterals :: String -> String -> Bool
hasMarkerOutsideLiterals marker = goNormal
  where
    goNormal [] = False
    goNormal s@(c:cs)
        | marker `L.isPrefixOf` s = True
        | c == '"' = goInString cs
        | c == '\' = goInChar cs
        | otherwise = goNormal cs
    
    goInString [] = False
    goInString ('"':cs) = goNormal cs
    goInString ('\\':_:cs) = goInString cs
    goInString (_:cs) = goInString cs
    
    goInChar [] = False
    goInChar ('\'':cs) = goNormal cs
    goInChar ('\\':_:cs) = goInChar cs
    goInChar (_:cs) = goInChar cs