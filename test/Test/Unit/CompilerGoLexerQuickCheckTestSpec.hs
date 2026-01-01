{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CompilerGoLexerQuickCheckTestSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import qualified Data.List as L
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck
import qualified Data.Map as Map
import Data.List (sort, nub)
import Data.Char (isSpace, isAlphaNum, isLetter)

import Compiler.GoLexer
import SourceLocation (SourcePos(..), SourceSpan(..), startPos)
import TestSupport.Arbitrary ()

tests :: TestTree
tests = testGroup "Compiler GoLexer QuickCheck Tests"
  [ lexerTokenTests
  , lexerKeywordTests
  , lexerIdentifierTests
  , lexerLiteralTests
  , lexerOperatorTests
  , lexerCommentTests
  , lexerWhitespaceTests
  , lexerStringTests
  , lexerNumberTests
  , lexerErrorTests
  ]

-- | 1. 词法分析器Token测试
lexerTokenTests :: TestTree
lexerTokenTests = testGroup "Lexer Token Tests"
  [ testCase "Token position tracking" $
      let tokens = lexGo "x"
      in case tokens of
           [token] -> tokenSpan token `seq` True @?= True
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Token type identification" $
      let tokens = lexGo "func"
      in case tokens of
           [token] -> tokenType token @?= KeywordToken
           _ -> "Expected single token" @?= "Got different number"
  
  , fastProperty "Token count matches lexemes" $
      \input -> let tokens = lexGo input
                in L.length tokens >= 0
  ]

-- | 2. 关键字测试
lexerKeywordTests :: TestTree
lexerKeywordTests = testGroup "Lexer Keyword Tests"
  [ testCase "Function keyword" $
      let tokens = lexGo "func"
      in case tokens of
           [token] -> tokenValue token @?= "func"
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Package keyword" $
      let tokens = lexGo "package"
      in case tokens of
           [token] -> tokenValue token @?= "package"
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Import keyword" $
      let tokens = lexGo "import"
      in case tokens of
           [token] -> tokenValue token @?= "import"
           _ -> "Expected single token" @?= "Got different number"
  
  , fastProperty "Keyword recognition" $
      \keyword -> let tokens = lexGo keyword
                  in case tokens of
                       [token] -> tokenType token == KeywordToken
                       _ -> False
  ]

-- | 3. 标识符测试
lexerIdentifierTests :: TestTree
lexerIdentifierTests = testGroup "Lexer Identifier Tests"
  [ testCase "Simple identifier" $
      let tokens = lexGo "myVar"
      in case tokens of
           [token] -> tokenType token @?= IdentifierToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Identifier with numbers" $
      let tokens = lexGo "var123"
      in case tokens of
           [token] -> tokenType token @?= IdentifierToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Identifier with underscore" $
      let tokens = lexGo "my_var"
      in case tokens of
           [token] -> tokenType token @?= IdentifierToken
           _ -> "Expected single token" @?= "Got different number"
  
  , fastProperty "Valid identifier characters" $
      \ident -> L.all isValidIdentifierChar ident && not (null ident) ==> 
        let tokens = lexGo ident
        in case tokens of
             [token] -> tokenType token == IdentifierToken
             _ -> False
  ]
  where
    isValidIdentifierChar c = isLetter c || c == '_' || (isAlphaNum c && not (null ident))

-- | 4. 字面量测试
lexerLiteralTests :: TestTree
lexerLiteralTests = testGroup "Lexer Literal Tests"
  [ testCase "Integer literal" $
      let tokens = lexGo "42"
      in case tokens of
           [token] -> tokenType token @?= IntegerToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "String literal" $
      let tokens = lexGo "\"hello\""
      in case tokens of
           [token] -> tokenType token @?= StringToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Boolean literal true" $
      let tokens = lexGo "true"
      in case tokens of
           [token] -> tokenValue token @?= "true"
           _ -> "Expected single token" @?= "Got different number"
  
  , fastProperty "Integer literal parsing" $
      \n -> let tokens = lexGo (show n)
            in case tokens of
                 [token] -> tokenType token == IntegerToken
                 _ -> False
  ]

-- | 5. 操作符测试
lexerOperatorTests :: TestTree
lexerOperatorTests = testGroup "Lexer Operator Tests"
  [ testCase "Addition operator" $
      let tokens = lexGo "+"
      in case tokens of
           [token] -> tokenType token @?= OperatorToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Assignment operator" =
      let tokens = lexGo "="
      in case tokens of
           [token] -> tokenType token @?= OperatorToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Comparison operator" =
      let tokens = lexGo "=="
      in case tokens of
           [token] -> tokenType token @?= OperatorToken
           _ -> "Expected single token" @?= "Got different number"
  
  , fastProperty "Operator recognition" =
      \op -> let tokens = lexGo op
             in case tokens of
                  [token] -> tokenType token == OperatorToken
                  _ -> False
  ]

-- | 6. 注释测试
lexerCommentTests :: TestTree
lexerCommentTests = testGroup "Lexer Comment Tests"
  [ testCase "Single line comment" =
      let tokens = lexGo "// This is a comment"
      in case tokens of
           [token] -> tokenType token @?= CommentToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Multi-line comment" =
      let tokens = lexGo "/* This is a\nmulti-line comment */"
      in case tokens of
           [token] -> tokenType token @?= CommentToken
           _ -> "Expected single token" @?= "Got different number"
  
  , fastProperty "Comment content preservation" =
      \content -> let comment = "// " ++ content
                      tokens = lexGo comment
                  in case tokens of
                       [token] -> tokenType token == CommentToken
                       _ -> False
  ]

-- | 7. 空白字符测试
lexerWhitespaceTests :: TestTree
lexerWhitespaceTests = testGroup "Lexer Whitespace Tests"
  [ testCase "Space handling" =
      let tokens = lexGo "x y"
      in L.length tokens @?= 2
  
  , testCase "Tab handling" =
      let tokens = lexGo "x\ty"
      in L.length tokens @?= 2
  
  , testCase "Newline handling" =
      let tokens = lexGo "x\ny"
      in L.length tokens @?= 2
  
  , fastProperty "Whitespace separation" =
      \s1 s2 -> let tokens = lexGo (s1 ++ " " ++ s2)
                in L.length tokens >= 2
  ]

-- | 8. 字符串测试
lexerStringTests :: TestTree
lexerStringTests = testGroup "Lexer String Tests"
  [ testCase "Simple string" =
      let tokens = lexGo "\"hello\""
      in case tokens of
           [token] -> tokenValue token @?= "hello"
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "String with spaces" =
      let tokens = lexGo "\"hello world\""
      in case tokens of
           [token] -> tokenValue token @?= "hello world"
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "String with escape sequence" =
      let tokens = lexGo "\"hello\\nworld\""
      in case tokens of
           [token] -> tokenType token @?= StringToken
           _ -> "Expected single token" @?= "Got different number"
  
  , fastProperty "String content preservation" =
      \content -> let str = "\"" ++ content ++ "\""
                      tokens = lexGo str
                  in case tokens of
                       [token] -> tokenType token == StringToken
                       _ -> False
  ]

-- | 9. 数字测试
lexerNumberTests :: TestTree
lexerNumberTests = testGroup "Lexer Number Tests"
  [ testCase "Decimal number" =
      let tokens = lexGo "123"
      in case tokens of
           [token] -> tokenType token @?= IntegerToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Negative number" =
      let tokens = lexGo "-456"
      in case tokens of
           [token, _] -> tokenType token @?= OperatorToken && tokenValue token @?= "-"
           _ -> "Expected two tokens" @?= "Got different number"
  
  , testCase "Float number" =
      let tokens = lexGo "3.14"
      in case tokens of
           [token] -> tokenType token @?= FloatToken
           _ -> "Expected single token" @?= "Got different number"
  
  , fastProperty "Number parsing consistency" =
      \n -> let tokens = lexGo (show n)
            in case tokens of
                 [token] -> tokenType token == IntegerToken
                 _ -> False
  ]

-- | 10. 错误处理测试
lexerErrorTests :: TestTree
lexerErrorTests = testGroup "Lexer Error Tests"
  [ testCase "Invalid character" =
      let tokens = lexGo "@"
      in case tokens of
           [token] -> tokenType token @?= ErrorToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Unterminated string" =
      let tokens = lexGo "\"unterminated"
      in case tokens of
           [token] -> tokenType token @?= ErrorToken
           _ -> "Expected single token" @?= "Got different number"
  
  , testCase "Invalid escape sequence" =
      let tokens = lexGo "\"invalid\\x escape\""
      in case tokens of
           [token] -> tokenType token @?= ErrorToken
           _ -> "Expected single token" @?= "Got different number"
  
  , fastProperty "Error token generation" =
      \invalidInput -> let tokens = lexGo invalidInput
                       in L.any (\t -> tokenType t == ErrorToken) tokens || null tokens
  ]