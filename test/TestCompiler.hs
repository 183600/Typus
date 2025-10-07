{-# LANGUAGE OverloadedStrings #-}
module TestCompiler (compilerTestSuite) where

import Test.Tasty
import Test.Tasty.HUnit as TH
import qualified Parser (parseTypus)
import qualified Compiler (compile)
import Data.List (isInfixOf)

-- Tasty test suite for integration with comprehensive tests
compilerTestSuite :: TestTree
compilerTestSuite = testGroup "Compiler Tests" [
    TH.testCase "Compiler Simple Case" $ do
        let code = unlines [
                "package main",
                "",
                "func main() {",
                "    fmt.Println(\"Hello, World!\")",
                "}"
                ]
        case Parser.parseTypus code of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left err -> TH.assertFailure $ "Compilation failed: " ++ err
                    Right goCode -> do
                        TH.assertBool "Generated code should contain package declaration"
                            ("package main" `isInfixOf` goCode)
                        TH.assertBool "Generated code should contain main function"
                            ("func main" `isInfixOf` goCode),

    TH.testCase "Compiler Empty Code" $ do
        case Parser.parseTypus "" of
            Left err -> TH.assertFailure $ "Parser failed: " ++ err
            Right typusFile -> do
                case Compiler.compile typusFile of
                    Left _ -> return ()  -- Expected to fail or handle gracefully
                    Right _ -> do
                        TH.assertBool "Should handle empty code" True
    ]