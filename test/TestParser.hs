{-# LANGUAGE OverloadedStrings #-}
module TestParser (parserTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit as TH
import qualified Parser (parseTypus, tfDirectives, tfBlocks, FileDirectives(..))

-- Tasty test suite for integration with comprehensive tests
parserTestSuite :: TestTree
parserTestSuite = testGroup "Parser Tests" [
    TH.testCase "Parser Basic Functionality" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    fmt.Println(\"Hello, World!\")",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed on simple Go code: " ++ err
            Right typusFile -> do
                TH.assertBool "Should parse package directive correctly"
                    (Parser.tfDirectives typusFile == Parser.FileDirectives Nothing Nothing Nothing)
                TH.assertBool "Should have at least one code block"
                    (not $ null $ Parser.tfBlocks typusFile),

    TH.testCase "Parser File Directives" $ do
        let code = unlines [
                "//! ownership: on",
                "//! dependent_types: on",
                "",
                "package main",
                "",
                "func main() {",
                "    fmt.Println(\"Hello, World!\")",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed on code with directives: " ++ err
            Right typusFile -> do
                let directives = Parser.tfDirectives typusFile
                TH.assertEqual "Ownership should be enabled" (Just True) (Parser.fdOwnership directives)
                TH.assertEqual "Dependent types should be enabled" (Just True) (Parser.fdDependentTypes directives),

    TH.testCase "Parser Empty Code" $ do
        case Parser.parseTypus "" of
            Left err -> TH.assertFailure $ "Parser failed on empty code: " ++ err
            Right typusFile -> do
                TH.assertEqual "Should have no blocks for empty code" [] (Parser.tfBlocks typusFile)
    ]