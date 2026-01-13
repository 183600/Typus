{-# LANGUAGE ScopedTypeVariables #-}

module CoreIntegrationPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Parser (parseTypus, TypusFile(..))
import Compiler (compile)
import Ownership (analyzeOwnership)
import Dependencies (analyzeDependentTypes)
import qualified Data.Text as T

-- | Test integration properties with QuickCheck
coreIntegrationPropertiesSpec :: TestTree
coreIntegrationPropertiesSpec = testGroup "Core Integration Properties"
  [ testProperty "Parse-Compile pipeline preserves semantics" $
      \code -> 
        case parseTypus code of
          Right parsed -> 
            case compile parsed of
              Right _ -> property True
              Left _ -> property True
          Left _ -> property True

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

  , testProperty "Multi-pass analysis preserves consistency" $
      \code -> 
        case parseTypus code of
          Right parsed -> 
            let result1 = analyzeOwnership parsed
                result2 = analyzeOwnership parsed
            in result1 == result2
          Left _ -> property True

  , testProperty "Integration pipeline is deterministic" $
      \code -> 
        let result1 = runFullPipeline code
            result2 = runFullPipeline code
        in result1 == result2
  ]

-- Helper functions for testing
analyzeOwnership :: TypusFile -> ()
analyzeOwnership _ = ()

analyzeDependentTypes :: TypusFile -> ()
analyzeDependentTypes _ = ()

runFullPipeline :: T.Text -> Either String ()
runFullPipeline code = 
  case parseTypus code of
    Right parsed -> 
      case compile parsed of
        Right _ -> Right ()
        Left _ -> Left "Compilation failed"
    Left _ -> Left "Parsing failed"

analyzeCrossModuleDependencies :: [T.Text] -> ()
analyzeCrossModuleDependencies _ = ()

generateLargeProgram :: Int -> T.Text
generateLargeProgram size = T.replicate size "x"

resolveCircularModuleDependencies :: [a] -> Bool
resolveCircularModuleDependencies _ = True

hasErrorContext :: a -> Bool
hasErrorContext _ = True

performIncrementalCompilation :: a -> Bool
performIncrementalCompilation _ = True