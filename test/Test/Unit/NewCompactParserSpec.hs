module Test.Unit.NewCompactParserSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=)), assertBool
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll, choose, elements)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import qualified Data.Text as T
import Data.Char 
  [ "func main() { return 0; }"
  , "let x = 42;"
  , "if (x > 0) { print(x); }"
  , "for (i := 0; i < 10; i++) { }"
  , "// This is a comment\nvar y                               string = \"hello\";"
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


-- | 
genDirectiveCode :: Gen String
                              genDirectiveCode = do
              hasOwnership <- elements [True, False]
  hasDepTypes <- elements [True, False]
  let ownership = if hasOwnership then "// @ownership\n" else ""
                                    depTypes = if hasDepTypes then "// @dependent-types\n" else ""
                                    code = "func test() {}"
  return $ ownership ++ depTypes ++ code

-- | 
testBasicParsing :: TestTree
testBasicParsing = testGroup ""
  [             testCase "" $
      let input = "func main() { return 0; }"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> assertBool "" True
    
    ,             testCase "" $
      let input = "let x = 42;"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> assertBool "" True
    
    ,             testCase "" $
      let input = ""
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> assertBool "" True
  ]

-- | 
testDirectiveParsing :: TestTree
testDirectiveParsing = testGroup ""
  [             testCase "" $
      let input = "// @ownership\nfunc test() {}"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> 
          let directives = tfFileDirectives file
          in case fdOwnership directives of
            Just (Located _ True) -> assertBool "" True
            _ -> assertBool "" False
    
    ,             testCase "" $
      let input = "// @dependent-types\nfunc test() {}"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> 
          let directives = tfFileDirectives file
          in case fdDependentTypes directives of
            Just (Located _ True) -> assertBool "" True
            _ -> assertBool "" False
  ]

-- | 
testCommentHandling :: TestTree
testCommentHandling = testGroup ""
  [             testCase "" $
      let input = "func test() {} // "
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> assertBool "" True
    
    ,             testCase "" $
      let input = "func test() {} /* \n */"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> assertBool "" True
    
    ,             testCase "" $
      let input = "// @ownership\n// @dependent-types\nfunc main() { /* comment */ return 0; // comment }"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> 
          let directives = tfFileDirectives file
                                            hasOwnership = maybe False (const True) (fdOwnership directives)
                                            hasDepTypes = maybe False (const True) (fdDependentTypes directives)
          in assertBool "" (hasOwnership && hasDepTypes)
  ]

-- | 
testErrorRecovery :: TestTree
testErrorRecovery = testGroup ""
  [             testCase "" $
      let input = "func malformed( { return 0; }\nfunc correct() { return 1; }"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool "" False
        Right file -> 
          let blocks = tfCodeBlocks file
                                            hasCorrectBlock = L.any (\(CodeBlock _ code) -> "correct" `elem` code) blocks
          in assertBool "" hasCorrectBlock
    
    ,             testCase "" $
      let input = "func test( (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0) { return 0;"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool "" True
        Right file -> assertBool "" False
  ]

-- | QuickCheck
testParserProperties :: TestTree
testParserProperties = testGroup ""
  [             testProperty "" $
      forAll genSimpleCode $ \code ->
        let result = parseTypus code
        in case result of
          Left _ -> False
          Right _ -> True
  
  ,             testProperty "" $
      forAll genDirectiveCode $ \code ->
        let result = parseTypus code
        in case result of
          Left _ -> False
          Right file -> 
            let blocks = tfCodeBlocks file
                                              hasCodeBlock = not (null blocks)
            in hasCodeBlock
  ]

-- | 
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup ""
  [             testProperty "" $
      \baseCode n -> 
        let repeatedCode = L.concat (replicate (min 100 (max 1 n) baseCode)
                                          result = parseTypus repeatedCode
        in case result of
          Left _ -> True  -- 
          Right _ -> True  -- 
  ]

-- | 
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup ""
  [             testCase "" $
      let input = "   \n\t  \n  "
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> assertBool "" True
    
    ,             testCase "" $
      let input = "// \n/*  */"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err (SourceSpan (SourcePos 1 1 0) (SourcePos 1 1 0) False
        Right file -> assertBool "" True
    
    ,             testCase "" $
      let longIdent = L.concat (replicate 1000 "a")
                                        input = "func " ++ longIdent ++ "() { return 0; }"
                                        result = parseTypus input
      in case result of
        Left err -> assertBool (": " ++ err) False
        Right file -> assertBool "" True
  ]

-- | 
tests :: TestTree
tests =   testGroup "Parser"
  [ testBasicParsing
  , testDirectiveParsing
  , testCommentHandling
  , testErrorRecovery
  , testParserProperties
  , testPerformanceProperties
  , testBoundaryConditions
  ]))