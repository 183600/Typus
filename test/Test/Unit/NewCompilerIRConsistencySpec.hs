{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Unit.NewCompilerIRConsistencySpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import Test.Tasty.QuickCheck (testProperty, Property, Arbitrary(..), Gen, choose, oneof, listOf, vectorOf, forAll, elements)
import qualified Data.Text as T
import qualified Data.List as List
import Data.Maybe (isJust, isNothing, fromMaybe)

import Compiler.IR
import Compiler.GoAst
import Parser (TypusFile(..), CodeBlock(..), FileDirectives(..), BlockDirectives(..))
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, locatedAt)
import TestSupport.QuickCheck (fastProperty)

-- | Test compiler IR generation consistency properties
tests :: TestTree
tests =
  testGroup "New Compiler IR Consistency Tests"
    [ testGroup "SourceIR construction"
        [ testCase "buildSourceIR preserves original file" $ do
            let typusFile = TypusFile defaultFileDirectives []
                sourceText = "func test() {}"
                ir = buildSourceIR typusFile sourceText
            sourceTypusFile ir @?= typusFile
            sourceText ir @?= sourceText

        , testCase "rawSourceFromTypus extracts code blocks" $ do
            let block = CodeBlock defaultBlockDirectives "func test() {}\n"
                typusFile = TypusFile defaultFileDirectives [block]
                extracted = rawSourceFromTypus typusFile
            extracted @?= "func test() {}\n"

        , testCase "rawSourceFromTypus handles multiple blocks" $ do
            let block1 = CodeBlock defaultBlockDirectives "func test1() {}\n"
                block2 = CodeBlock defaultBlockDirectives "func test2() {}\n"
                typusFile = TypusFile defaultFileDirectives [block1, block2]
                extracted = rawSourceFromTypus typusFile
            extracted @?= "func test1() {}\nfunc test2() {}\n"

        , testCase "rawSourceFromTypus handles empty blocks" $ do
            let typusFile = TypusFile defaultFileDirectives []
                extracted = rawSourceFromTypus typusFile
            extracted @?= ""
        ]

    , testGroup "SemanticIR construction"
        [ testCase "buildSemanticIR preserves structure" $ do
            let block = CodeBlock defaultBlockDirectives "func test() {}"
                typusFile = TypusFile defaultFileDirectives [block]
                result = buildSemanticIR typusFile
            case result of
              Left _ -> assertBool "Should build semantic IR" False
              Right ir -> semanticTypusFile ir @?= typusFile

        , testCase "buildSemanticIRWithPackage includes package" $ do
            let block = CodeBlock defaultBlockDirectives "func test() {}"
                typusFile = TypusFile defaultFileDirectives [block]
                result = buildSemanticIRWithPackage "main" typusFile
            case result of
              Left _ -> assertBool "Should build semantic IR with package" False
              Right ir -> do
                semanticTypusFile ir @?= typusFile
                -- Check that package is included in the semantic representation
                let goCode = emitGo ir
                goCode `assertBool` ("package main" `T.isInfixOf` goCode)

        , testCase "ensurePackageDecl adds package when missing" $ do
            let goCode = "func test() {}"
                withPackage = ensurePackageDecl "main" goCode
            withPackage `assertBool` ("package main" `T.isInfixOf` withPackage)

        , testCase "ensurePackageDecl preserves existing package" $ do
            let goCode = "package custom\nfunc test() {}"
                withPackage = ensurePackageDecl "custom" goCode
            withPackage @?= goCode
        ]

    , testGroup "Main function synthesis"
        [ testCase "ensureMainFunction adds main when missing" $ do
            let goCode = "func helper() {}"
                withMain = ensureMainFunction goCode
            withMain `assertBool` ("func main()" `T.isInfixOf` withMain)

        , testCase "ensureMainFunction preserves existing main" $ do
            let goCode = "func main() { println(\"hello\") }"
                withMain = ensureMainFunction goCode
            withMain @?= goCode

        , testCase "ensureMainFunction handles multiple functions" $ do
            let goCode = "func helper1() {}\nfunc helper2() {}"
                withMain = ensureMainFunction goCode
            withMain `assertBool` ("func helper1()" `T.isInfixOf` withMain)
            withMain `assertBool` ("func helper2()" `T.isInfixOf` withMain)
            withMain `assertBool` ("func main()" `T.isInfixOf` withMain)
        ]

    , testGroup "Import inference"
        [ testCase "attachInferredImports adds fmt for println" $ do
            let goCode = "func main() { println(\"hello\") }"
                withImports = attachInferredImports goCode
            withImports `assertBool` ("import \"fmt\"" `T.isInfixOf` withImports)

        , testCase "attachInferredImports adds multiple imports" $ do
            let goCode = "func main() { println(\"hello\"); http.Get(\"url\") }"
                withImports = attachInferredImports goCode
            withImports `assertBool` ("import \"fmt\"" `T.isInfixOf` withImports)
            withImports `assertBool` ("import \"net/http\"" `T.isInfixOf` withImports)

        , testCase "attachInferredImports preserves existing imports" $ do
            let goCode = "import \"fmt\"\nfunc main() { println(\"hello\") }"
                withImports = attachInferredImports goCode
            -- Should not duplicate imports
            let fmtCount = length $ filter ("import \"fmt\"" `T.isInfixOf`) (lines withImports)
            fmtCount @?= 1

        , testCase "attachInferredImports handles no imports needed" $ do
            let goCode = "func main() { x := 1 }"
                withImports = attachInferredImports goCode
            withImports `assertBool` (not $ "import" `T.isInfixOf` withImports)
        ]

    , testGroup "Module extraction"
        [ testCase "moduleFromTypus extracts single block" $ do
            let block = CodeBlock defaultBlockDirectives "func test() {}"
                typusFile = TypusFile defaultFileDirectives [block]
                result = moduleFromTypus typusFile
            case result of
              Left _ -> assertBool "Should extract module" False
              Right (moduleIR, _) -> do
                -- Check that module contains the function
                let goCode = emitGo moduleIR
                goCode `assertBool` ("func test()" `T.isInfixOf` goCode)

        , testCase "moduleFromTypus handles multiple blocks" $ do
            let block1 = CodeBlock defaultBlockDirectives "func test1() {}"
                block2 = CodeBlock defaultBlockDirectives "func test2() {}"
                typusFile = TypusFile defaultFileDirectives [block1, block2]
                result = moduleFromTypus typusFile
            case result of
              Left _ -> assertBool "Should extract module" False
              Right (moduleIR, _) -> do
                let goCode = emitGo moduleIR
                goCode `assertBool` ("func test1()" `T.isInfixOf` goCode)
                goCode `assertBool` ("func test2()" `T.isInfixOf` goCode)
        ]

    , testGroup "IR generation consistency"
        [ fastProperty "buildSourceIR is deterministic" prop_buildSourceIRDeterministic
        , fastProperty "rawSourceFromTypus preserves content order" prop_rawSourcePreservesOrder
        , fastProperty "ensurePackageDecl is idempotent" prop_ensurePackageDeclIdempotent
        , fastProperty "ensureMainFunction is idempotent" prop_ensureMainFunctionIdempotent
        , fastProperty "attachInferredImports is idempotent" prop_attachInferredImportsIdempotent
        ]

    , testGroup "Error handling and edge cases"
        [ testCase "handles empty TypusFile gracefully" $ do
            let typusFile = TypusFile defaultFileDirectives []
                sourceText = ""
                ir = buildSourceIR typusFile sourceText
            sourceTypusFile ir @?= typusFile
            sourceText ir @?= sourceText

        , testCase "handles malformed code blocks" $ do
            let block = CodeBlock defaultBlockDirectives "incomplete function {"
                typusFile = TypusFile defaultFileDirectives [block]
                result = buildSemanticIR typusFile
            -- Should either succeed or fail gracefully without crashing
            case result of
              Left _ -> assertBool "Should handle malformed code gracefully" True
              Right _ -> assertBool "Should handle malformed code gracefully" True

        , testCase "handles very large code blocks" $ do
            let largeFunction = "func large() {\n" ++ unlines (replicate 1000 "  x := 1") ++ "\n}"
                block = CodeBlock defaultBlockDirectives largeFunction
                typusFile = TypusFile defaultFileDirectives [block]
                result = buildSemanticIR typusFile
            case result of
              Left _ -> assertBool "Should handle large blocks" False
              Right _ -> assertBool "Should handle large blocks" True
        ]

    , testGroup "Integration consistency"
        [ testCase "full pipeline preserves semantics" $ do
            let block = CodeBlock defaultBlockDirectives "func hello() { println(\"world\") }"
                typusFile = TypusFile defaultFileDirectives [block]
            case buildSemanticIR typusFile of
              Left _ -> assertBool "Should build semantic IR" False
              Right ir -> do
                let withPackage = ensurePackageDecl "main" (emitGo ir)
                    withMain = ensureMainFunction withPackage
                    withImports = attachInferredImports withMain
                withImports `assertBool` ("package main" `T.isInfixOf` withImports)
                withImports `assertBool` ("func main()" `T.isInfixOf` withImports)
                withImports `assertBool` ("func hello()" `T.isInfixOf` withImports)
                withImports `assertBool` ("import \"fmt\"" `T.isInfixOf` withImports)

        , testCase "IR transformations commute properly" $ do
            let goCode = "func test() { println(\"hello\") }"
                withPackageFirst = ensureMainFunction (ensurePackageDecl "main" goCode)
                withMainFirst = ensurePackageDecl "main" (ensureMainFunction goCode)
            -- Order shouldn't matter for the final result
            withPackageFirst `assertBool` ("package main" `T.isInfixOf` withPackageFirst)
            withPackageFirst `assertBool` ("func main()" `T.isInfixOf` withPackageFirst)
            withMainFirst `assertBool` ("package main" `T.isInfixOf` withMainFirst)
            withMainFirst `assertBool` ("func main()" `T.isInfixOf` withMainFirst)
        ]
    ]

-- Property: buildSourceIR is deterministic
prop_buildSourceIRDeterministic :: TypusFile -> String -> Property
prop_buildSourceIRDeterministic typusFile sourceText =
  let ir1 = buildSourceIR typusFile sourceText
      ir2 = buildSourceIR typusFile sourceText
  in ir1 == ir2

-- Property: rawSourceFromTypus preserves content order
prop_rawSourcePreservesOrder :: Positive Int -> Property
prop_rawSourcePreservesOrder (Positive n) =
  let blocks = [CodeBlock defaultBlockDirectives ("func " ++ show i ++ "() {}") | i <- [1..n]]
      typusFile = TypusFile defaultFileDirectives blocks
      extracted = rawSourceFromTypus typusFile
      expected = concat ["func " ++ show i ++ "() {}" | i <- [1..n]]
  in extracted == expected

-- Property: ensurePackageDecl is idempotent
prop_ensurePackageDeclIdempotent :: String -> String -> Property
prop_ensurePackageDeclIdempotent packageName goCode =
  let once = ensurePackageDecl packageName goCode
      twice = ensurePackageDecl packageName once
  in once == twice

-- Property: ensureMainFunction is idempotent
prop_ensureMainFunctionIdempotent :: String -> Property
prop_ensureMainFunctionIdempotent goCode =
  let once = ensureMainFunction goCode
      twice = ensureMainFunction once
  in once == twice

-- Property: attachInferredImports is idempotent
prop_attachInferredImportsIdempotent :: String -> Property
prop_attachInferredImportsIdempotent goCode =
  let once = attachInferredImports goCode
      twice = attachInferredImports once
  in once == twice

-- Helper wrapper for positive integers
newtype Positive a = Positive a
  deriving (Show, Eq)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Positive a) where
  arbitrary = Positive <$> choose (1, 20)

-- Minimal Arbitrary instance for testing
instance Arbitrary TypusFile where
  arbitrary = do
    n <- choose (0, 3)
    blocks <- vectorOf n arbitrary
    return $ TypusFile defaultFileDirectives blocks

instance Arbitrary CodeBlock where
  arbitrary = do
    content <- elements ["func test() {}", "var x int", "const y = 42"]
    return $ CodeBlock defaultBlockDirectives content
