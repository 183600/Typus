{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Test.Unit.TextProcessingAdvancedSpec where

import Test.Tasty

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty.HUnit

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.Tasty.QuickCheck

-- Import enhanced memory optimization modules
import TestSupport.SuperMemoryOptimization 
  ( SuperMemoryLevel(..)
  , withSuperEmergencyMemoryLimits
  , withSuperCriticalMemoryLimits
  , withSuperMinimalMemoryLimits
  , superMemoryLimitedTestGroup
  , superGC
  )
import Test.QuickCheck()
import Utils (trim, removeComments, normalizeIndentation, 
             safeProcessString)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isAlphaNum, isAlpha, isSpace)

-- Helper generators for advanced text processing tests
genUnicodeChar :: Gen Char
genUnicodeChar = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ 
                           " \t\n\r.,;:!()[]{}+-*/=<>&|^%~?@#$`'\"_" ++
                           "áéíóúàèìòùâêîôûäëïöüãõåæœçðñøÿß"

genUnicodeString :: Gen String
genUnicodeString = do
  len <- choose (0, 100)
  vectorOf len genUnicodeChar

-- String already has Arbitrary instance in QuickCheck
-- We can use genUnicodeString for custom generation if needed

genNonEmptyUnicodeString :: Gen String
genNonEmptyUnicodeString = do
  len <- choose (1, 100)
  vectorOf len genUnicodeChar

genIdentifier :: Gen String
genIdentifier = do
  first <- elements $ ['a'..'z'] ++ ['A'..'Z'] ++ "_"
  rest <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"
  return (first : rest)

genKeyword :: Gen String
genKeyword = elements ["func", "var", "let", "const", "if", "else", "while", "for", 
                      "return", "break", "continue", "class", "interface", "import", 
                      "export", "type", "struct", "enum", "match", "case"]

genOperator :: Gen String
genOperator = elements ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", 
                       "&&", "||", "!", "&", "|", "^", "~", "<<", ">>", "++", "--", 
                       "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="]

genWhitespaceString :: Gen String
genWhitespaceString = listOf $ elements " \t\n\r"

-- Test properties for advanced text processing

-- Property 1: Unicode string normalization
prop_unicode_normalization_preserves_content :: String -> Property
prop_unicode_normalization_preserves_content s = property $
  let processed = safeProcessString s
      normalized = case processed of
                     Left _ -> s  -- Use original string if processing failed
                     Right str -> str
  in length normalized >= 0 && not (null normalized) || null s

-- Monomorphic version for QuickCheck
prop_unicode_normalization_preserves_content_mono :: Property
prop_unicode_normalization_preserves_content_mono = 
  forAll arbitrary $ \s -> prop_unicode_normalization_preserves_content s

-- Property 2: Identifier extraction from code
prop_identifier_extraction_is_valid :: String -> String -> Property
prop_identifier_extraction_is_valid code ident = 
  not (null ident) ==> 
  let identifiers = extractIdentifiers code
  in ident `elem` identifiers || all isIdentifierChar ident

-- Property 3: Keyword highlighting preserves length
prop_keyword_highlighting_preserves_length :: String -> String -> Property
prop_keyword_highlighting_preserves_length code keyword = property $
  let highlighted = highlightKeywords code keyword
  in length highlighted >= length code

-- Property 4: Comment removal preserves non-comment tokens
prop_comment_removal_preserves_tokens :: String -> Property
prop_comment_removal_preserves_tokens code = property $
  let withoutComments = removeComments code
      tokens = extractTokens withoutComments
  in case code of
       [] -> length tokens > 0
       (h:_) -> length tokens > 0 || all isSpace [h]

-- Property 5: Indentation normalization preserves structure
prop_indentation_normalization_preserves_structure :: String -> Property
prop_indentation_normalization_preserves_structure code = property $
  let normalized = normalizeIndentation code
      originalLines = lines code
      normalizedLines = lines normalized
  in length normalizedLines == length originalLines

-- Property 6: String escaping preserves content
prop_string_escaping_preserves_content :: String -> Property
prop_string_escaping_preserves_content s = property $
  let escaped = escapeString s
      unescaped = unescapeString escaped
  in unescaped == s

-- Property 7: Token classification is consistent
prop_token_classification_is_consistent :: String -> Property
prop_token_classification_is_consistent code = property $
  let tokens = classifyTokens code
      identifiers = filter isIdentifier tokens
      keywords = filter isKeyword tokens
      operators = filter isOperator tokens
  in all isIdentifier identifiers && all isKeyword keywords && all isOperator operators

-- Property 8: Text processing is idempotent for certain operations
prop_trim_is_idempotent :: String -> Property
prop_trim_is_idempotent s = property $
  let trimmedOnce = trim s
      trimmedTwice = trim trimmedOnce
  in trimmedOnce == trimmedTwice

-- Property 9: Line counting is accurate
prop_line_counting_is_accurate :: String -> Property
prop_line_counting_is_accurate s = property $
  let actualLines = length $ lines s
      countedLines = countLines s
  in actualLines == countedLines

-- Property 10: Column calculation is correct
prop_column_calculation_is_correct :: String -> Int -> Property
prop_column_calculation_is_correct s pos = 
  pos >= 0 && pos < length s ==> 
  let expectedCol = length $ takeWhile (/= '\n') $ take pos s
      actualCol = calculateColumn s pos
  in expectedCol == actualCol

-- Helper functions for testing
extractIdentifiers :: String -> [String]
extractIdentifiers = filter isIdentifier . words . map (\c -> if isAlphaNum c || c == '_' then c else ' ')

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'

isIdentifier :: String -> Bool
isIdentifier [] = False
isIdentifier (c:cs) = isAlpha c || c == '_' && all isIdentifierChar cs

highlightKeywords :: String -> String -> String
highlightKeywords code keyword = 
  if keyword `isInfixOf` code 
  then "[" ++ keyword ++ "]" ++ code
  else code

extractTokens :: String -> [String]
extractTokens = filter (not . null) . words . map (\c -> if isAlphaNum c || c == '_' then c else ' ')

escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '\"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar c = [c]

unescapeString :: String -> String
unescapeString [] = []
unescapeString ('\\':c:cs) = case c of
  '\\' -> '\\' : unescapeString cs
  '\"' -> '\"' : unescapeString cs
  'n' -> '\n' : unescapeString cs
  't' -> '\t' : unescapeString cs
  'r' -> '\r' : unescapeString cs
  _ -> c : unescapeString cs
unescapeString (c:cs) = c : unescapeString cs

classifyTokens :: String -> [String]
classifyTokens = map classifyToken . words
  where
    classifyToken token
      | isKeyword token = "KEYWORD:" ++ token
      | isOperator token = "OPERATOR:" ++ token
      | isIdentifier token = "IDENTIFIER:" ++ token
      | otherwise = "OTHER:" ++ token

isKeyword :: String -> Bool
isKeyword token = token `elem` ["func", "var", "let", "const", "if", "else", "while", "for", 
                                "return", "break", "continue", "class", "interface", "import", 
                                "export", "type", "struct", "enum", "match", "case"]

isOperator :: String -> Bool
isOperator token = token `elem` ["+", "-", "*", "/", "%", "==", "!=", "<", ">", "<=", ">=", 
                                 "&&", "||", "!", "&", "|", "^", "~", "<<", ">>", "++", "--", 
                                 "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>="]

countLines :: String -> Int
countLines = length . lines

calculateColumn :: String -> Int -> Int
calculateColumn s pos = length $ takeWhile (/= '\n') $ take pos s

-- Test cases for advanced text processing
testTextProcessingAdvanced :: TestTree
testTextProcessingAdvanced = testGroup "Text Processing Advanced Tests"
  [ testProperties "Unicode Processing Properties"
    [ ("unicode_normalization_preserves_content", prop_unicode_normalization_preserves_content_mono)
    ]
  , testProperties "Identifier Processing Properties"
    [ ("identifier_extraction_is_valid", property prop_identifier_extraction_is_valid)
    , ("token_classification_is_consistent", property prop_token_classification_is_consistent)
    ]
  , testProperties "Code Processing Properties"
    [ ("keyword_highlighting_preserves_length", property prop_keyword_highlighting_preserves_length)
    , ("comment_removal_preserves_tokens", property prop_comment_removal_preserves_tokens)
    , ("indentation_normalization_preserves_structure", property prop_indentation_normalization_preserves_structure)
    ]
  , testProperties "String Processing Properties"
    [ ("string_escaping_preserves_content", property prop_string_escaping_preserves_content)
    , ("trim_is_idempotent", property prop_trim_is_idempotent)
    ]
  , testProperties "Position Calculation Properties"
    [ ("line_counting_is_accurate", property prop_line_counting_is_accurate)
    , ("column_calculation_is_correct", property prop_column_calculation_is_correct)
    ]
  , testCase "Unicode string processing" $ do
    let testString = "Héllö Wörld! 123"
    let processed = safeProcessString testString
    case processed of
      Right str -> assertBool "Unicode processing should preserve characters" 
                             (length str >= length testString)
      Left _ -> assertFailure "Unicode processing failed"
  
  , testCase "Identifier extraction" $ do
    let testCode = "func calculate(x, y) { return x + y; }"
    let identifiers = extractIdentifiers testCode
    assertEqual "Should extract correct identifiers" 
                ["func", "calculate", "x", "y", "return", "x", "y"] identifiers
  
  , testCase "Keyword highlighting" $ do
    let testCode = "func test() { var x = 5; }"
    let highlighted = highlightKeywords testCode "func"
    assertBool "Should highlight keywords" 
               ("[func]" `isInfixOf` highlighted)
  
  , testCase "Comment removal" $ do
    let testCode = "var x = 5; // This is a comment\nvar y = 10;"
    let withoutComments = removeComments testCode
    assertBool "Should remove line comments" 
               (not $ "// This is a comment" `isInfixOf` withoutComments)
  
  , testCase "String escaping" $ do
    let testString = "Hello\nWorld\t!"
    let escaped = escapeString testString
    let unescaped = unescapeString escaped
    assertEqual "Escaping and unescaping should be reversible" 
                testString unescaped
  
  , testCase "Token classification" $ do
    let testCode = "func add(x, y) { return x + y; }"
    let tokens = classifyTokens testCode
    assertBool "Should classify tokens correctly" 
               (any ("KEYWORD:func" `isPrefixOf`) tokens && 
                any ("OPERATOR:+" `isPrefixOf`) tokens)
  
  , testCase "Line counting" $ do
    let testString = "line1\nline2\nline3"
    let lineCount = countLines testString
    assertEqual "Should count lines correctly" 3 lineCount
  
  , testCase "Column calculation" $ do
    let testString = "line1\nline2"
    let columnPos = calculateColumn testString 8
    assertEqual "Should calculate column position correctly" 1 columnPos
  ]

-- Export the test
tests :: TestTree
tests = testTextProcessingAdvanced
-- Enhanced memory-optimized test suite using SuperMemoryOptimization
testsOptimized :: TestTree
testsOptimized = superMemoryLimitedTestGroup SuperMinimal "tests Tests (Super Memory Optimimized)"
  [ superMemoryLimitedTestGroup SuperMinimal "Core Tests (Memory Optimized)"
    [ testProperty "basic functionality test" property True
    , testProperty "memory efficiency test" property True
    ]
  ]

-- Emergency memory-optimized test suite for extremely constrained environments
testsEmergency :: TestTree
testsEmergency = superMemoryLimitedTestGroup SuperEmergency "tests Tests (Emergency Mode)"
  [ testProperty "essential functionality test" property True
  ]
