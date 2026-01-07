module Test.Unit.NewParserBoundaryQuickCheckTestSpec where
import Test.QuickCheck 
import Test.Tasty.QuickCheck (Property,             testProperty, Arbitrary(..), Gen, oneof, elements, listOf, listOf1, suchThat)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..), CodeBlock(..), TypusFile(..), defaultFileDirectives, defaultBlockDirectives)
import SourceLocation (Located(..), SourcePos(..), SourceSpan)
                        let input = "func test_123$special() {\n  return true\n}"
                                              result = parseTypus input
            case result of
                Left _ -> @?= False True
                Right file -> @?= True True
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


          ,             testCase "parseTypus handles mixed line endings" $ do
                        let input = "func test() {\n\r\n  return true\n}"
                                              result = parseTypus input
            case result of
                Left _ -> @?= False True
                Right file -> @?= True True
        ]
    ]

-- | parseTypus
prop_parseEmptyInput :: Property
                              prop_parseEmptyInput = 
  let result = parseTypus ""
  in case result of
       Left _ -> False -- Should not fail on empty input
       Right file -> True

-- | parseTypus
prop_parseWhitespaceOnly :: String -> Property
prop_parseWhitespaceOnly                               s =
  let whitespaceOnly = L.all isSpace s
                                    result = parseTypus s
  in                               whitespaceOnly ==> case result of
                          Left _ -> False
                          Right file -> True

-- | parseTypus
prop_parseLongIdentifiers :: Property
                              prop_parseLongIdentifiers =
  let longIdent = replicate 1000 'a' ++ "123"
                                    input = "func " ++ longIdent ++ "() { return true }"
                                    result = parseTypus input
  in case result of
       Left _ -> False -- Should handle long identifiers
       Right file -> True

-- | parseTypus
prop_parseDeeplyNested :: Property
                              prop_parseDeeplyNested =
  let depth = 100
      createNestedBlock                               0 = "return true"
      createNestedBlock                               n = "if true { " ++ createNestedBlock (n-1) ++ " }"
                                    input = "func deeplyNested() { " ++ createNestedBlock depth ++ " }"
                                    result = parseTypus input
  in case result of
       Left _ -> depth <= 50 -- May fail for very deep nesting, but should handle reasonable depth
       Right file -> True

-- | parseTypus
prop_parseMalformedDirectives :: String -> Property
prop_parseMalformedDirectives                               s =
  let malformedDirective = "// @ownership: maybe\n// @dependent-types: \n// @invalid-directive: true"
                                    input = malformedDirective ++ "\nfunc test() { return true }"
                                    result = parseTypus input
  in case result of
       Left _ -> False -- Should not crash on malformed directives
       Right file -> True

-- | 
prop_fileDirectivesParsing :: Bool -> Bool -> Bool -> Property
prop_fileDirectivesParsing ownership dependent                               constraints =
  let input = "// @ownership: " ++ show ownership ++ "\n" ++
             "// @dependent-types: " ++ show dependent ++ "\n" ++
             "// @constraints: " ++ show constraints ++ "\n" ++
             "func test() { return true }"
                                    result = parseTypus input
  in case result of
       Left _ -> False
       Right file -> True -- Should parse successfully

-- | 
prop_blockDirectivesOverride :: Property
                              prop_blockDirectivesOverride =
  let input = "// @ownership: true\n" ++
             "func test() {\n" ++
             "  // @ownership: false\n" ++
             "  return true\n" ++
             "}\n"
                                    result = parseTypus input
  in case result of
       Left _ -> False
       Right file -> True

-- | 
prop_directivesCaseSensitive :: Property
                              prop_directivesCaseSensitive =
  let input = "// @OWNERSHIP: true\n// @Ownership: false\nfunc test() { return true }"
                                    result = parseTypus input
  in case result of
       Left _ -> False -- Should not fail on case variations
       Right file -> True

-- | 
prop_invalidDirectivesIgnored :: Property
                              prop_invalidDirectivesIgnored =
  let input = "// @invalid-directive: true\n// @another-invalid: false\nfunc test() { return true }"
                                    result = parseTypus input
  in case result of
       Left _ -> False
       Right file -> True

-- | 
prop_parseErrorLocations :: Property
                              prop_parseErrorLocations =
  let input = "func invalid syntax { return true }"
                                    result = parseTypus input
  in case result of
       Left err -> L.length (show err) > 0 -- Error message should not be empty
       Right _ -> True

-- | 
prop_unclosedBlocksDetected :: Property
                              prop_unclosedBlocksDetected =
  let input = "func unclosed() {\n  if true {\n    return true\n  // Missing closing braces"
                                    result = parseTypus input
  in case result of
       Left _ -> True -- Should detect unclosed blocks
       Right _ -> False

-- | 
prop_mismatchedDirectivesCaught :: Property
                              prop_mismatchedDirectivesCaught =
  let input = "// @ownership: not-a-boolean\nfunc test() { return true }"
                                    result = parseTypus input
  in case result of
       Left _ -> True -- Should catch invalid directive values
       Right _ -> False

-- | 
prop_parseLargeFiles :: Property
                              prop_parseLargeFiles =
  let largeContent = unlines $ replicate 10000 "func line" ++ show ++ "() { return true }"
                                    input = largeContent ++ "\nfunc main() { return true }"
                                    result = parseTypus input
  in case result of
       Left _ -> False -- Should handle large files
       Right file -> True

-- | 
prop_manyDirectivesStable :: Property
                              prop_manyDirectivesStable =
  let manyDirectives = unlines $ replicate 100 "// @ownership: true"
                                    input = manyDirectives ++ "\nfunc test() { return true }"
                                    result = parseTypus input
  in case result of
       Left _ -> False
       Right file -> True

-- | unicode
prop_unicodeContentParsing :: Property
                              prop_unicodeContentParsing =
  let unicodeInput = "func unicode() {\n  //  with mojis \n  let  = \"hello \"\n  return \n}"
                                    result =  parseTypus unicodeInput
  in property $ case result of
       Left _ -> False -- Should handle unicode content
       Right file -> True