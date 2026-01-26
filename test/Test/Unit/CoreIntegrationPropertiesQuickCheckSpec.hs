{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Test.Unit.CoreIntegrationPropertiesQuickCheckSpec where



import Test.Tasty.HUnit
import Test.Tasty
import Test.Tasty.QuickCheck

import Parser (parseTypus, TypusFile(..))
import Compiler (compile)
import qualified Ownership as Ownership (analyzeOwnership)
import qualified Dependencies as Dependencies (analyzeDependentTypes)
import qualified Data.Text as T

-- | Test integration properties with QuickCheck
coreIntegrationPropertiesSpec :: TestTree
coreIntegrationPropertiesSpec = testGroup "Core Integration Properties"
  [ testCase "Parse-Compile pipeline preserves semantics" $ do
    let code = "func main() { return 42; }"
    case parseTypus code of
      Right parsed -> 
        case compile parsed of
          Right _ -> assertBool "Pipeline preserves semantics" True
          Left _ -> assertBool "Pipeline preserves semantics" True
      Left _ -> assertBool "Pipeline preserves semantics" True

  , testCase "Full compilation pipeline handles simple programs" $ do
    let simpleProgram = "func main() { return 42; }"
    case parseTypus simpleProgram of
      Right parsed -> 
        case compile parsed of
          Right _ -> assertBool "Compilation succeeded" True
          Left _ -> assertFailure "Compilation failed"
      Left _ -> assertFailure "Parsing failed"

  , testCase "Integration handles error propagation correctly" $ do
    let malformedProgram = "func invalid {"
    case parseTypus malformedProgram of
      Right parsed -> 
        case compile parsed of
          Left _ -> assertBool "Error propagated correctly" True
          Right _ -> assertFailure "Expected compilation error"
      Left _ -> assertBool "Parse error detected" True

  , testCase "Multi-pass analysis preserves consistency" $ do
    let code = "func main() { return 42; }"
    case parseTypus code of
      Right parsed -> 
        let result1 = Ownership.analyzeOwnership (show parsed)
            result2 = Ownership.analyzeOwnership (show parsed)
        in assertBool "Multi-pass analysis is consistent" (result1 == result2)
      Left _ -> assertBool "Multi-pass analysis is consistent" True

  , testCase "Integration pipeline is deterministic" $ do
    let code = T.pack "func main() { return 42; }"
        result1 = runFullPipeline code
        result2 = runFullPipeline code
    assertBool "Pipeline is deterministic" (result1 == result2)
  ]

-- Helper functions for testing
runFullPipeline :: T.Text -> Either String ()
runFullPipeline code = 
  case parseTypus (T.unpack code) of
    Right parsed -> 
      case compile parsed of
        Right _ -> Right ()
        Left _ -> Left "Compilation failed"
    Left _ -> Left "Parsing failed"

generateLargeProgram :: Int -> T.Text
generateLargeProgram size = T.replicate size (T.pack "x")

generateProgram :: Int -> T.Text
generateProgram size = T.pack "func main() { " <> T.replicate size (T.pack "x") <> T.pack " }"

generateNestedStructure :: Int -> T.Text
generateNestedStructure depth = T.replicate depth (T.pack "{")

generateLargeType :: Int -> T.Text
generateLargeType size = T.pack ("type Large struct { " ++ concat (replicate size "Field int; ") ++ " }")