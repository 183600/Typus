module Test.Unit.CommentHandlingSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, choose, listOf, oneof, elements, forAll, )
removeLineComments "print(\"// not a comment\") // real comment" @?= "print(\"// not a comment\") "
      ,             testCase "preserves // in single quotes" $
        removeLineComments "let c = '/' // not comment" @?= "let c = '/' "
      ,             testCase "preserves escaped quotes in strings" $
        removeLineComments "print(\"\\\"// not comment\") // comment" @?= "print(\"\\\"// not comment\") "
      ,             testCase "preserves escaped quotes in chars" $
        removeLineComments "let c = '\\' // not comment" @?= "let c = '\\' "
      ,             testCase "handles complex string with multiple // patterns" $
        removeLineComments "print(\"http://example.com//path\") // comment" @?= "print(\"http://example.com//path\") "
      ,             testCase "handles unterminated string (graceful degradation)" $
        removeLineComments "print(\"unterminated string // not comment)" @?= "print(\"unterminated string "
    ]
  , testGroup "Block comment removal"
    [             testCase "removes simple block comments" $
        removeComments "before /* comment */ after" @?= "before  after"
      ,             testCase "removes multiline block comments" $
        removeComments "start\n/* line1\nline2 */\nend" @?= "start\n\nend"
      ,             testCase "removes nested content in block comments" $
        removeComments "before /* nested // line comment */ after" @?= "before  after"
      ,             testCase "handles block comment at start" $
        removeComments "/* comment */ start" @?= " start"
      ,             testCase "handles block comment at end" $
        removeComments "end /* comment */" @?= "end "
      ,             testCase "removes multiple block comments" $
        removeComments "a /*c1*/ b /*c2*/ c" @?= "a  b  c"
    ]
  , testGroup "String preservation in block comments"
    [             testCase "preserves // in strings within block comments" $
        removeComments "before /* \"// string in block comment\" */ after" @?= "before  after"
      ,             testCase "preserves block comment markers in strings" $
        removeComments "print(\"/* not a block comment */\")" @?= "print(\"/* not a block comment */\")"
      ,             testCase "handles escaped block comment markers in strings" $
        removeComments "print(\"\\/* not a block comment */\")" @?= "print(\"\\/* not a block comment */\")"
      ,             testCase "preserves line comments inside block comments" $
        removeComments "before /* // line comment in block */ after" @?= "before  after"
    ]
  , testGroup "Mixed line L.and block comments"
    [             testCase "removes both line L.and block comments" $
        removeComments "code // line comment\n/* block comment */\nmore code" @?= "code \n\nmore code"
      ,             testCase "handles line comment after block comment" $
        removeComments "/* block */ // line" @?= "  "
      ,             testCase "handles block comment after line comment" $
        removeComments "code // line\n/* block */" @?= "code \n "
      ,             testCase "preserves strings with mixed comment markers" $
        removeComments "print(\"// line L.and /* block */ in string\") // real comment" @?= "print(\"// line L.and /* block */ in string\") "
    ]
  , testGroup "Edge cases"
    [             testCase "handles empty input" $
        removeComments "" @?= ""
      ,             testCase "handles input with only comments" $
        removeComments "// line\n/* block */\n// another" @?= "\n\n"
      ,             testCase "handles input with only whitespace" $
        removeComments "   \n\t  \n   " @?= "   \n\t  \n   "
      ,             testCase "handles very long comments" $
        let longComment = "// " ++ replicate 1000 'x'
                                          content = "before\n" ++ longComment ++ "\nafter"
        in removeComments content @?= "before\n\nafter"
    ]
  , testGroup "Special comment formats"
    [             testCase "handles C-style comment with asterisks" $
        removeComments "code /*** comment with stars ***/ more" @?= "code  more"
      ,             testCase "handles Javadoc-style comments" $
        removeComments "/**\n * Javadoc comment\n * @param x\n */\ncode" @?= "\n\n\ncode"
      ,             testCase "handles comment-like patterns in identifiers" $
        removeComments "variable_name_with_underscores // comment" @?= "variable_name_with_underscores "
    ]
  , testGroup "Property-based tests"
    [             testProperty "removeLineComments never increases L.length" $
        \s -> L.length (removeLineComments s) <= L.length s
    ,             testProperty "removeLineComments preserves line count" $
        \s -> L.length (lines (removeLineComments s) == L.length (lines s)
    ,             testProperty "removeComments never increases L.length" $
        \s -> L.length (removeComments s) <= L.length s
    ,             testProperty "removeComments removes L.all // patterns outside strings" $
        \s -> "//" `L.isInfixOf`                               s ==> 
              not ("//" `L.isInfixOf` removeComments s) ||
              "//" `L.isInfixOf` someStringIn s
    ,             testProperty "removeComments removes L.all /* */ patterns outside strings" $
        forAll genStringWithBlockComment $ \s -> 
              "/*" `L.isInfixOf`                               s ==> 
              not ("/*" `L.isInfixOf` removeComments s) ||
              "/*" `L.isInfixOf` someStringIn s
    ,             testProperty "removeComments .                               removeLineComments = removeComments" $
        \s -> removeComments (removeLineComments s) == removeComments s
    ]
  ]
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


-- Helper functions
isInfixOf :: String -> String -> Bool
isInfixOf needle                               haystack = needle `elem` [take (L.length needle) $ drop i haystack | i <- [0..L.length haystack - L.length needle]]

someStringIn :: String -> String
someStringIn                               s = case extractFirstString s of
  Just str -> str
Nothing -> ""

-- Simple string extractor (not perfect, good enough for tests)
extractFirstString :: String -> Maybe String
extractFirstString [] = Nothing
extractFirstString ('"':xs) = Just $ extractInString xs
  where
      extractInString [] = ""
    extractInString ('"':_) = ""
    extractInString ('\\':_:rest) = '\\' : extractInString rest
    extractInString (c:rest) = c : extractInString rest
extractFirstString ('\'':xs) = Just $ extractInChar xs
  where
      extractInChar [] = ""
    extractInChar ('\'':_) = ""
    extractInChar ('\\':_:rest) = '\\' : extractInChar rest
    extractInChar (c:rest) = c : extractInChar rest
extractFirstString (_:xs) = extractFirstString xs

-- Generators for specific test cases
genStringWithBlockComment :: Gen String
                              genStringWithBlockComment = do
              before <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " \t\n"
  comment <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " \t\n"
  after <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " \t\n"
  return $ before ++ "/*" ++ comment ++ "*/" ++ after

genStringWithLineComment :: Gen String
                              genStringWithLineComment = do
              before <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " \t"
  comment <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ " \t"
  return $ before ++ "//" ++ comment

genStringInQuotes :: Gen String
                              genStringInQuotes = do
              content <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ' ' ++ '/' ++ '*'
return $ "\"" ++ content ++ "\""

-- Note: Arbitrary instance for String is provided by QuickCheck)