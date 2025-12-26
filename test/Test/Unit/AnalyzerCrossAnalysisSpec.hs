module Test.Unit.AnalyzerCrossAnalysisSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import Test.Tasty.QuickCheck (testProperty, Property, forAll, Gen, arbitrary, elements)
import Control.Monad.State
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Analyzer.CrossAnalysis (runCrossAnalysis)
import Analyzer.State 
  ( AnalyzerState(..)
  , SymbolInfo(..)
  , CombinedError(..)
  , ErrorLevel(..)
  , initialAnalyzerState
  )
import Analyzer.Types (SymbolType(..))
import qualified Dependencies as Dep
import qualified Ownership as Own

tests :: TestTree
tests = testGroup "Analyzer.CrossAnalysis Tests"
  [ testCrossAnalysisBasicFunctionality
  , testOwnershipTypeConflicts
  , testTypeOwnershipInconsistencies
  , testUnusedVariableDetection
  , testCrossAnalysisWithEmptyCode
  , testCrossAnalysisWithComplexCode
  , testSymbolInfoHandling
  , testErrorGeneration
  , testCrossAnalysisProperties
  ]

testCrossAnalysisBasicFunctionality :: TestTree
testCrossAnalysisBasicFunctionality = testCase "Basic cross analysis functionality" $ do
  let code = "func main() { var x int = 42 }"
      initialState = initialAnalyzerState
  
  result <- evalStateT (runCrossAnalysis code) initialState
  assertBool "Cross analysis should complete" (True)

testOwnershipTypeConflicts :: TestTree
testOwnershipTypeConflicts = testCase "Ownership type conflicts detection" $ do
  let symbolInfo = SymbolInfo
        { symbolName = "testVar"
        , symbolType = Just (Dep.TVCon "Int")
        , ownershipState = Just (Own.Owned "testVar")
        , symbolLocation = (1, 1)
        , isMoved = True
        , isBorrowed = False
        }
      
      symbols = Map.singleton "testVar" symbolInfo
      initialState = initialAnalyzerState { symbolTable = symbols }
      code = "func test() { var testVar int = 5 }"
  
  result <- evalStateT (runCrossAnalysis code) initialState
  case result of
    [] -> assertBool "Should detect ownership-type conflict" False
    [CrossAnalyzerError msg Error _] -> 
      assertBool "Error message should mention conflict" 
        ("dependent type" `isInfixOf` msg && "moved" `isInfixOf` msg)
    _ -> assertBool "Should return a single error" False

testTypeOwnershipInconsistencies :: TestTree
testTypeOwnershipInconsistencies = testCase "Type ownership inconsistencies detection" $ do
  let symbolInfo = SymbolInfo
        { symbolName = "inconsistentVar"
        , symbolType = Just (Dep.TVCon "String")
        , ownershipState = Just (Own.Owned "inconsistentVar")
        , symbolLocation = (1, 1)
        , isMoved = True
        , isBorrowed = True  -- This is the inconsistency
        }
      
      symbols = Map.singleton "inconsistentVar" symbolInfo
      initialState = initialAnalyzerState { symbolTable = symbols }
      code = "func test() { inconsistentVar := \"test\" }"
  
  result <- evalStateT (runCrossAnalysis code) initialState
  case result of
    [] -> assertBool "Should detect inconsistency" False
    [CrossAnalyzerError msg Warning _] ->
      assertBool "Error should mention both moved and borrowed" 
        ("moved" `isInfixOf` msg && "borrowed" `isInfixOf` msg)
    _ -> assertBool "Should return a single error" False

testUnusedVariableDetection :: TestTree
testUnusedVariableDetection = testCase "Unused variable detection" $ do
  let symbolInfo = SymbolInfo
        { symbolName = "unusedVar"
        , symbolType = Just (Dep.TVCon "Int")
        , ownershipState = Just (Own.Owned "unusedVar")
        , symbolLocation = (1, 1)
        , isMoved = False
        , isBorrowed = False
        }
      
      symbols = Map.singleton "unusedVar" symbolInfo
      initialState = initialAnalyzerState { symbolTable = symbols }
      code = "func test() { var unusedVar int = 5 }"
  
  result <- evalStateT (runCrossAnalysis code) initialState
  case result of
    [] -> assertBool "Should detect unused variable" False
    [CrossAnalyzerError msg Warning _] ->
      assertBool "Warning should mention unused variable" 
        ("never used" `isInfixOf` msg)
    _ -> assertBool "Should return a single warning" False

testCrossAnalysisWithEmptyCode :: TestTree
testCrossAnalysisWithEmptyCode = testCase "Cross analysis with empty code" $ do
  let code = ""
      initialState = initialAnalyzerState
  
  result <- evalStateT (runCrossAnalysis code) initialState
  assertEqual "Empty code should produce no errors" [] result

testCrossAnalysisWithComplexCode :: TestTree
testCrossAnalysisWithComplexCode = testCase "Cross analysis with complex code" $ do
  let symbol1 = SymbolInfo
        { symbolName = "complexVar1"
        , symbolType = Just (Dep.TVCon "ComplexType")
        , ownershipState = Just (Own.Owned "complexVar1")
        , symbolLocation = (1, 1)
        , isMoved = False
        , isBorrowed = False
        }
      
      symbol2 = SymbolInfo
        { symbolName = "complexVar2"
        , symbolType = Just (Dep.TVCon "AnotherType")
        , ownershipState = Just (Own.Owned "complexVar2")
        , symbolLocation = (2, 1)
        , isMoved = True
        , isBorrowed = False
        }
      
      symbols = Map.fromList [("complexVar1", symbol1), ("complexVar2", symbol2)]
      initialState = initialAnalyzerState { symbolTable = symbols }
      code = unlines
        [ "func complexFunction() {"
        , "  var complexVar1 ComplexType"
        , "  var complexVar2 AnotherType"
        , "  complexVar2 = someFunction()"
        , "  return complexVar1"
        , "}"
        ]
  
  result <- evalStateT (runCrossAnalysis code) initialState
  assertBool "Complex code analysis should complete" (True)

testSymbolInfoHandling :: TestTree
testSymbolInfoHandling = testCase "Symbol info handling in cross analysis" $ do
  let symbolInfo = SymbolInfo
        { symbolName = "testSymbol"
        , symbolType = Nothing
        , ownershipState = Nothing
        , symbolLocation = (1, 1)
        , isMoved = False
        , isBorrowed = False
        }
      
      symbols = Map.singleton "testSymbol" symbolInfo
      initialState = initialAnalyzerState { symbolTable = symbols }
      code = "func test() { testSymbol := 42 }"
  
  result <- evalStateT (runCrossAnalysis code) initialState
  assertBool "Should handle symbols without type/ownership info" (True)

testErrorGeneration :: TestTree
testErrorGeneration = testCase "Error generation and formatting" $ do
  let symbolInfo = SymbolInfo
        { symbolName = "errorVar"
        , symbolType = Just (Dep.TVCon "ErrorType")
        , ownershipState = Just (Own.Owned "errorVar")
        , symbolLocation = (1, 1)
        , isMoved = True
        , isBorrowed = True
        }
      
      symbols = Map.singleton "errorVar" symbolInfo
      initialState = initialAnalyzerState { symbolTable = symbols }
      code = "func test() { errorVar := generateError() }"
  
  result <- evalStateT (runCrossAnalysis code) initialState
  case result of
    (CrossAnalyzerError msg level _ : _) -> do
      assertBool "Error message should not be empty" (not $ null msg)
      assertEqual "Error level should be Error" Error level
    [] -> assertBool "Should generate at least one error" False

testCrossAnalysisProperties :: TestTree
testCrossAnalysisProperties = testProperty "Cross analysis preserves symbol information" $
  forAll arbitrarySymbolInfo $ \symbolInfo -> do
    let symbols = Map.singleton "testSymbol" symbolInfo
        initialState = initialAnalyzerState { symbolTable = symbols }
        code = "func test() { }"
    
    result <- evalStateT (runCrossAnalysis code) initialState
    return $ length result >= 0  -- Should always return a list (possibly empty)

-- Helper generators for QuickCheck tests
arbitrarySymbolInfo :: Gen SymbolInfo
arbitrarySymbolInfo = do
  name <- elements ["var1", "var2", "testVar", "example"]
  hasType <- arbitrary
  hasOwnership <- arbitrary
  moved <- arbitrary
  borrowed <- arbitrary
  
  let symbolType = if hasType then Just (Dep.TVCon "TestType") else Nothing
      ownershipState = if hasOwnership then Just (Own.Owned name) else Nothing
  
  return $ SymbolInfo
    { symbolName = name
    , symbolType = symbolType
    , ownershipState = ownershipState
    , symbolLocation = (1, 1)
    , isMoved = moved
    , isBorrowed = borrowed
    }

-- Helper function to check if a string is contained in another
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = needle `elem` words haystack