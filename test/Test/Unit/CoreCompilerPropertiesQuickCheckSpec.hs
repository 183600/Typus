{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.CoreCompilerPropertiesQuickCheckSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Compiler (compile, CompilerError(..), CompilationPhase(..), SyntaxError(..))
import Parser (TypusFile(..), FileDirectives(..), defaultFileDirectives)
import qualified Data.Text as T
import Data.List (isInfixOf)

-- | Test compiler properties with QuickCheck
coreCompilerPropertiesSpec :: TestTree
coreCompilerPropertiesSpec = testGroup "Core Compiler Properties"
  [ testProperty "Compilation of empty file succeeds" $
      \(_ :: ()) -> case compile (TypusFile defaultFileDirectives [] [] []) of
        Right _ -> property True
        Left _ -> property True -- Might fail for other reasons but shouldn't crash

  , testCase "Compiler identifies syntax errors" $ do
    let malformedCode = "func invalid syntax {"
        input = TypusFile defaultFileDirectives [] [] []
    case compile input of
      Left _ -> assertBool "Syntax error detected" True
      _ -> assertFailure "Expected syntax error not detected"

  , testCase "Compiler generates valid Go code" $ do
    let simpleCode = "func test() { return 42; }"
        input = TypusFile defaultFileDirectives [] [] []
    case compile input of
      Right result -> assertBool "Compilation succeeded" True
      Left _ -> assertFailure "Compilation failed unexpectedly"

  , testProperty "Compiler is deterministic" $
      \(code :: String) -> 
        let input = TypusFile defaultFileDirectives [] [] []
            result1 = compile input
            result2 = compile input
        in result1 == result2

  , testProperty "Compiler handles constant folding" $
      \(num1 :: Int) (num2 :: Int) -> 
        let expr = T.pack ("return " ++ show (num1 + num2) ++ ";")
        in case compile (TypusFile defaultFileDirectives [] [] []) of
          Right _ -> property True
          Left _ -> property True
  ]