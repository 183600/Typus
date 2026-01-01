{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalCompilerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, isLetter)
import Data.Maybe (isJust, isNothing)

import Compiler (compileTypus, CompilationResult(..))
import Parser (parseTypus)
import SourceLocation (SourceSpan(..))

-- Simple arbitrary instances for compiler testing
newtype SimpleGoCode = SimpleGoCode String deriving (Show, Eq)

instance Arbitrary SimpleGoCode where
  arbitrary = do
    return $ SimpleGoCode "package main\n\nfunc main() {\n    return 42\n}"

newtype SimpleExpression = SimpleExpression String deriving (Show, Eq)

instance Arbitrary SimpleExpression where
  arbitrary = oneof
    [ return $ SimpleExpression "42"
    , return $ SimpleExpression "\"hello\""
    , return $ SimpleExpression "true"
    , return $ SimpleExpression "x + y"
    ]

-- Property: Compilation preserves basic structure
prop_compilation_preserves_basic_structure :: SimpleExpression -> Property
prop_compilation_preserves_basic_structure (SimpleExpression expr) =
  let typusCode = "func main() { return " ++ expr ++ " }"
  in case parseTypus typusCode of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case compileTypus parsed of
           Left err -> counterexample ("Compilation failed: " ++ err) $ property False
           Right result -> property $ not $ L.null $ crGoCode result

-- Property: Compilation generates valid Go package declaration
prop_compilation_generates_package :: SimpleExpression -> Property
prop_compilation_generates_package (SimpleExpression expr) =
  let typusCode = "func main() { return " ++ expr ++ " }"
  in case parseTypus typusCode of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case compileTypus parsed of
           Left err -> counterexample ("Compilation failed: " ++ err) $ property False
           Right result -> 
             let goCode = crGoCode result
             in property $ "package main" `List.L.isInfixOf` goCode

-- Property: Compilation handles multiple functions
prop_compilation_handles_multiple_functions :: Property
prop_compilation_handles_multiple_functions =
  let typusCode = unlines
        [ "func add(x int, y int) int { return x + y }"
        , "func main() { return add(1, 2) }"
        ]
  in case parseTypus typusCode of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case compileTypus parsed of
           Left err -> counterexample ("Compilation failed: " ++ err) $ property False
           Right result -> 
             let goCode = crGoCode result
                 hasAdd = "func add" `List.L.isInfixOf` goCode
                 hasMain = "func main" `List.L.isInfixOf` goCode
             in property $ hasAdd .&&. hasMain

-- Property: Compilation preserves return types
prop_compilation_preserves_return_types :: SimpleExpression -> Property
prop_compilation_preserves_return_types (SimpleExpression expr) =
  let typusCode = "func test() int { return " ++ expr ++ " }"
  in case parseTypus typusCode of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case compileTypus parsed of
           Left err -> counterexample ("Compilation failed: " ++ err) $ property False
           Right result -> 
             let goCode = crGoCode result
             in property $ "int" `List.L.isInfixOf` goCode

tests :: TestTree
tests = testGroup "Cabal Compiler QuickCheck Tests"
  [ fastProperty "Compilation preserves basic structure" prop_compilation_preserves_basic_structure
  , fastProperty "Compilation generates package declaration" prop_compilation_generates_package
  , fastProperty "Compilation handles multiple functions" prop_compilation_handles_multiple_functions
  , fastProperty "Compilation preserves return types" prop_compilation_preserves_return_types
  , testCase "Compiler handles ownership annotations" $ do
      let source = unlines
            [ "//! ownership: on"
            , "func transfer_data() {"
            , "    let data = String{\"hello\"}"
            , "    return data"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed: " ++ err
        Right parsed -> 
          case compileTypus parsed of
            Left err -> assertFailure $ "compileTypus failed: " ++ err
            Right result -> do
              let goCode = crGoCode result
              assertFailure $ "Compilation succeeded with code: " ++ take 100 goCode
  ]