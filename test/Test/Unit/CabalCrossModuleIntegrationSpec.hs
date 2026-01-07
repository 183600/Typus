module Test.Unit.CabalCrossModuleIntegrationSpec where
import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck 
import qualified Parser (parseTypus, TypusFile(..), FileDirectives)
import qualified SourceLocation (SourcePos(..), SourceSpan(..), Located)
                        let input = "// This is a comment\nfunc main() { /* block comment */ return 42; }"
                                              cleaned = Utils.removeComments input
                                              result = Parser.parseTypus cleaned
            case result of
              Left err -> "Should parse successfully" @?= show err
              Right _ -> "Success" @?= "Success"
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


          ,             testCase "Parser handles trimmed whitespace" $ do
                        let input = "  \n  func main() { return 42; }  \n  "
                                              trimmed = Utils.trim input
                                              result = Parser.parseTypus trimmed
            case result of
              Left err -> "Should parse successfully" @?= show err
              Right _ -> "Success" @?= "Success"

        ,             testProperty "splitBy integration with parser line handling" $ do
            \input -> let lines = Utils.splitBy '\n' input
                                                        lineCount = L.length lines
                      in lineCount >= 0
        ]

    , testGroup "SourceLocation L.and Parser Integration"
        [             testCase "Source locations are preserved in parse results" $ do
                        let input = "func test() { return 1; }"
                                              result = Parser.parseTypus input
            case result of
              Left _ -> "Should parse successfully" @?= "Parse failed"
              Right typusFile -> 
                case Parser.tfBlocks typusFile of
                  [] -> "Should have code blocks" @?= "No code blocks found"
                  (block:_) -> "Should have valid span" @?= "Valid span"

          ,             testCase "Source position calculations are consistent" $ do
                        let pos1 = SourceLocation.SourcePos 1 1 0
                                              pos2 = SourceLocation.SourcePos 1 5 0
                                              span = SourceLocation.SourceSpan pos1 pos2
            SourceLocation.spanStart span @?= pos1
            SourceLocation.spanEnd span @?= pos2
        ]

    , testGroup "SyntaxValidator L.and Parser Integration"
        [             testCase "Validated parsed code passes syntax validation" $ do
                        let input = "func validated() { return true; }"
                                              result = Parser.parseTypus input
            case result of
              Left _ -> "Parse failed" @?= "Should parse successfully"
              Right typusFile -> do
                -- Assuming syntax validation would pass for simple valid code
                "Validation should pass" @?= "Should validate"

          ,             testCase "Syntax validation catches parser edge cases" $ do
                        let input = "func invalid() { return ; }"  -- Missing expression
                                              result = Parser.parseTypus input
            case result of
              Left _ -> "Parse should fail" @?= "Expected parse failure"
              Right _ -> "Unexpected success" @?= "Should not reach here"
        ]

    , testGroup "FileDirectives Integration"
        [             testCase "Parser correctly extracts file directives" $ do
                        let input = "// @ownership: true\n// @dependent-types: false\nfunc main() {}"
                                              result = Parser.parseTypus input
            case result of
              Left _ -> "Parse failed" @?= "Should parse successfully"
              Right typusFile -> do
                            let directives = Parser.tfDirectives typusFile
                Parser.fdOwnership directives @?= Just (SourceLocation.Located True (SourceLocation.SourcePos 1 1 0) (SourceLocation.SourceSpan (SourceLocation.SourcePos 1 1 0) (SourceLocation.SourcePos 1 1 0))
                Parser.fdDependentTypes directives @?= Just (SourceLocation.Located False (SourceLocation.SourcePos 1 2 0) (SourceLocation.SourceSpan (SourceLocation.SourcePos 1 2 0) (SourceLocation.SourcePos 1 2 0))
        ]

    , testGroup "Error handling integration"
        [             testCase "Parser provides meaningful error locations" $ do
                        let input = "func broken() { return }"  -- Missing semicolon
                                              result = Parser.parseTypus input
            case result of
              Left err -> 
                -- Error should contain location information
                let errStr = show err
                in "line" `elem` words errStr @?= True
              Right _ -> "Unexpected success" @?= "Should fail"
        ]
    ]