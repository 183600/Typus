{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.CabalAnalyzerQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.))
import qualified Data.List as List
import Data.Char (isSpace, isAlphaNum, isLetter, toLower, toUpper)
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Data.Map as Map

import Analyzer (analyzeProgram, AnalysisResult(..), SymbolInfo(..), AnalysisError(..))
import Analyzer.SymbolTable (SymbolTable(..))
import Parser (parseTypus)
import SourceLocation (SourceSpan(..), mkSourceSpan, mkSourcePos)

-- Simple arbitrary instances for analyzer testing
newtype VariableName = VariableName String deriving (Show, Eq)

instance Arbitrary VariableName where
  arbitrary = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
    return $ VariableName (first : rest)

newtype FunctionName = FunctionName String deriving (Show, Eq)

instance Arbitrary FunctionName where
  arbitrary = do
    first <- elements ['a'..'z']
    rest <- listOf $ elements (['a'..'z'] ++ ['0'..'9'] ++ ['_'])
    return $ FunctionName (first : rest)

data VarUsage = Declare | Use | Reassign deriving (Show, Eq)

instance Arbitrary VarUsage where
  arbitrary = elements [Declare, Use, Reassign]

-- Property: Analyzer detects variable declarations
prop_analyzer_detects_declarations :: VariableName -> Property
prop_analyzer_detects_declarations (VariableName var) =
  let code = "func test() { let " ++ var ++ " = 42; return " ++ var ++ " }"
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case analyzeProgram parsed of
           Right result -> 
             let symbols = arSymbolTable result
                 hasVar = Map.member var symbols
             in property $ hasVar
           Left _ -> property False  -- Analysis failure is acceptable for complex cases

-- Property: Analyzer tracks function definitions
prop_analyzer_tracks_functions :: FunctionName -> Property
prop_analyzer_tracks_functions (FunctionName func) =
  let code = "func " ++ func ++ "() { return 42 }"
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case analyzeProgram parsed of
           Right result -> 
             let symbols = arSymbolTable result
                 hasFunc = Map.member func symbols
             in property $ hasFunc
           Left _ -> property False

-- Property: Analyzer detects variable usage patterns
prop_analyzer_detects_usage_patterns :: [(VariableName, VarUsage)] -> Property
prop_analyzer_detects_usage_patterns patterns =
  let varNames = L.map (\(VariableName v, _) -> v) patterns
      uniqueVars = List.nub varNames
      codeLines = ["func test() {"]
      codeBody = concatMap (\(VariableName v, usage) ->
                             case usage of
                               Declare -> ["let " ++ v ++ " = 42;"]
                               Use -> ["let _ = " ++ v ++ ";"]
                               Reassign -> [v ++ " = 24;"]
                           ) patterns
      code = unlines $ codeLines ++ codeBody ++ ["return 0; }"]
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case analyzeProgram parsed of
           Right result -> 
             let symbols = arSymbolTable result
                 foundVars = Map.keys symbols
             in property $ L.all (`elem` foundVars) uniqueVars
           Left _ -> property False

-- Property: Analyzer preserves type information
prop_analyzer_preserves_types :: VariableName -> Property
prop_analyzer_preserves_types (VariableName var) =
  let code = "func test() { let " ++ var ++ ": int = 42; return " ++ var ++ " }"
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case analyzeProgram parsed of
           Right result -> 
             let symbols = arSymbolTable result
                 varInfo = Map.lookup var symbols
             in case varInfo of
                  Just info -> property $ "int" `List.L.isInfixOf` show (siType info)
                  Nothing -> property False
           Left _ -> property False

-- Property: Analyzer detects scope boundaries
prop_analyzer_detects_scopes :: VariableName -> Property
prop_analyzer_detects_scopes (VariableName var) =
  let code = unlines
        [ "func test() {"
        , "    let " ++ var ++ " = 42;"
        , "    {"
        , "        let " ++ var ++ "_inner = 24;"
        , "        return " ++ var ++ " + " ++ var ++ "_inner;"
        , "    }"
        , "}"
        ]
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case analyzeProgram parsed of
           Right result -> 
             let symbols = arSymbolTable result
                 hasOuter = Map.member var symbols
                 hasInner = Map.member (var ++ "_inner") symbols
             in property $ hasOuter .&&. hasInner
           Left _ -> property False

-- Property: Analyzer handles cross-references
prop_analyzer_handles_cross_references :: [VariableName] -> Property
prop_analyzer_handles_cross_references vars =
  let varNames = L.map (\(VariableName v) -> v) vars
      codeLines = ["func test() {"]
      declarations = L.map (\v -> "let " ++ v ++ " = 42;") varNames
      usage = if null varNames then "return 0;" 
              else "return " ++ List.intercalate " + " varNames ++ ";"
      code = unlines $ codeLines ++ declarations ++ [usage ++ "}"]
  in case parseTypus code of
       Left err -> counterexample ("Parse failed: " ++ err) $ property False
       Right parsed -> 
         case analyzeProgram parsed of
           Right result -> 
             let symbols = arSymbolTable result
                 foundVars = Map.keys symbols
             in property $ L.all (`elem` foundVars) varNames
           Left _ -> property False

tests :: TestTree
tests = testGroup "Cabal Analyzer QuickCheck Tests"
  [ fastProperty "Analyzer detects declarations" prop_analyzer_detects_declarations
  , fastProperty "Analyzer tracks functions" prop_analyzer_tracks_functions
  , fastProperty "Analyzer detects usage patterns" prop_analyzer_detects_usage_patterns
  , fastProperty "Analyzer preserves types" prop_analyzer_preserves_types
  , fastProperty "Analyzer detects scopes" prop_analyzer_detects_scopes
  , fastProperty "Analyzer handles cross-references" prop_analyzer_handles_cross_references
  , testCase "Analyzer handles complex program structure" $ do
      let source = unlines
            [ "package main"
            , ""
            , "func factorial(n: int) int {"
            , "    if n <= 1 {"
            , "        return 1"
            , "    }"
            , "    return n * factorial(n - 1)"
            , "}"
            , ""
            , "func main() {"
            , "    let result = factorial(5)"
            , "    return result"
            , "}"
            ]
      case parseTypus source of
        Left err -> assertFailure $ "parseTypus failed: " ++ err
        Right parsed -> 
          case analyzeProgram parsed of
            Left err -> assertFailure $ "analyzeProgram failed: " ++ show err
            Right result -> do
              let symbols = arSymbolTable result
              assertFailure $ "Analysis succeeded with symbols: " ++ show (Map.keys symbols)
  ]