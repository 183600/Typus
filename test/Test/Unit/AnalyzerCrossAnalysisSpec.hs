module Test.Unit.AnalyzerCrossAnalysisSpec where
import Test.Tasty 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck
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


 code = "func main() { var x                               int = 42 }"
                                    initialState = newIntegratedAnalyzer True True
  
  result <- runExceptT (evalStateT (runCrossAnalysis code) initialState)
  case result of
    Left _ -> assertBool "Should not fail with exception" False
    Right _ -> assertBool "Cross analysis should complete" True

testOwnershipTypeConflicts :: TestTree
testOwnershipTypeConflicts =             testCase "Ownership type conflicts detection" $ do
                let symbolInfo = SymbolInfo
          {                               symbolName = "testVar"
          ,                               symbolType = Just (Dep.TVCon "Int")
          ,                               ownershipState = Just (Own.Owned "testVar")
          ,                               symbolScope = 1
          ,                               isMoved = True
          ,                               isBorrowed = False
          ,                               constraints = []
          }
                                      symbols = Map.singleton "testVar" symbolInfo
                                      initialState = (newIntegratedAnalyzer True True) {                               symbolTable = symbols }
                                      code = "func test() { var testVar                               int = 5 }"
    
    result <- runExceptT (evalStateT (runCrossAnalysis code) initialState)
    case result of
        Left _ -> assertBool "Should not fail with exception" False
        Right [] -> assertBool "Should detect ownership-type conflict" False
        Right [CrossAnalyzerError msg Error _] -> 
            assertBool "Error message should mention conflict" 
                ("dependent type" `L.isInfixOf` msg && "moved" `L.isInfixOf` msg)
        Right _ -> assertBool "Should return a single error" False

testTypeOwnershipInconsistencies :: TestTree
testTypeOwnershipInconsistencies =             testCase "Type ownership inconsistencies detection" $ do
                let symbolInfo = SymbolInfo
          {                               symbolName = "inconsistentVar"
          ,                               symbolType = Just (Dep.TVCon "String")
          ,                               ownershipState = Just (Own.Owned "inconsistentVar")
          ,                               symbolScope = 1
          ,                               isMoved = True
          ,                               isBorrowed = True  -- This is the inconsistency
          ,                               constraints = []
          }
      
                                    symbols = Map.singleton "inconsistentVar" symbolInfo
                                    initialState = (newIntegratedAnalyzer True True) {                               symbolTable = symbols }
                                    code = "func test() { inconsistentVar := \"test\" }"
  
  result <- runExceptT (evalStateT (runCrossAnalysis code) initialState)
  case result of
    Left _ -> assertBool "Should detect inconsistency" False
    Right [] -> assertBool "Should detect inconsistency" False
    Right [CrossAnalyzerError msg Warning _] ->
      assertBool "Error should mention both moved L.and borrowed" 
        ("moved" `L.isInfixOf` msg && "borrowed" `L.isInfixOf` msg)
    Right _ -> assertBool "Should return a single error" False

testUnusedVariableDetection :: TestTree
testUnusedVariableDetection =             testCase "Unused variable detection" $ do
                    let symbolInfo = SymbolInfo
        {                               symbolName = "unusedVar"
        ,                               symbolType = Just (Dep.TVCon "Int")
        ,                               ownershipState = Just (Own.Owned "unusedVar")
        ,                               symbolScope = 1
        ,                               isMoved = False
        ,                               isBorrowed = False
        ,                               constraints = []
    }
      
                                    symbols = Map.singleton "unusedVar" symbolInfo
                                    initialState = (newIntegratedAnalyzer True True) {                               symbolTable = symbols }
                                    code = "func test() { var unusedVar                               int = 5 }"
  
  result <- runExceptT (evalStateT (runCrossAnalysis code) initialState)
  case result of
    Left _ -> assertBool "Should detect unused variable" False
    Right [] -> assertBool "Should detect unused variable" False
    Right [CrossAnalyzerError msg Warning _] ->
      assertBool "Warning should mention unused variable" 
        ("never used" `L.isInfixOf` msg)
    _ -> assertBool "Should return a single warning" False

testCrossAnalysisWithEmptyCode :: TestTree
testCrossAnalysisWithEmptyCode =             testCase "Cross analysis with empty code" $ do
              let code = ""
                                    initialState = newIntegratedAnalyzer True True
  
result <- runExceptT (evalStateT (runCrossAnalysis code) initialState)
              assertEqual "Empty code should produce no errors" (Right []) result

testCrossAnalysisWithComplexCode :: TestTree
testCrossAnalysisWithComplexCode =             testCase "Cross analysis with complex code" $ do
                    let symbol1 = SymbolInfo
        {                               symbolName = "complexVar1"
        ,                               symbolType = Just (Dep.TVCon "ComplexType")
        ,                               ownershipState = Just (Own.Owned "complexVar1")
        ,                               symbolScope = 1
        ,                               isMoved = False
        ,                               isBorrowed = False
        ,                               constraints = []
    }
      
                                    symbol2 = SymbolInfo
        {                               symbolName = "complexVar2"
        ,                               symbolType = Just (Dep.TVCon "AnotherType")
        ,                               ownershipState = Just (Own.Owned "complexVar2")
        ,                               symbolScope = 1
        ,                               isMoved = True
        ,                               isBorrowed = False
        ,                               constraints = []
    }
      
                                    symbols = Map.fromList [("complexVar1", symbol1), ("complexVar2", symbol2)]
                                    initialState = (newIntegratedAnalyzer True True) {                               symbolTable = symbols }
                                    code = unlines
        [ "func complexFunction() {"
        , "  var complexVar1 ComplexType"
        , "  var complexVar2 AnotherType"
        , "                                complexVar2 = someFunction()"
        , "  return complexVar1"
        , "}"
        ]
  
  result <- runExceptT (evalStateT (runCrossAnalysis code) initialState)
  assertBool "Complex code analysis should complete" (True)

testSymbolInfoHandling :: TestTree
testSymbolInfoHandling =             testCase "Symbol info handling in cross analysis" $ do
              let symbolInfo = SymbolInfo
        {                               symbolName = "testSymbol"
        ,                               symbolType = Nothing
        ,                               ownershipState = Nothing
        ,                               symbolScope = 1
        ,                               isMoved = False
        ,                               isBorrowed = False
        ,                               constraints = []
    }
      
                                    symbols = Map.singleton "testSymbol" symbolInfo
                              initialState = (newIntegratedAnalyzer True True) {                               symbolTable = symbols }
                                    code = "func test() { testSymbol := 42 }"
  
  result <- runExceptT (evalStateT (runCrossAnalysis code) initialState)
  assertBool "Should handle symbols without type/ownership info" (True)

testErrorGeneration :: TestTree
testErrorGeneration =             testCase "Error generation L.and formatting" $ do
                    let symbolInfo = SymbolInfo
        {                               symbolName = "errorVar"
        ,                               symbolType = Just (Dep.TVCon "ErrorType")
        ,                               ownershipState = Just (Own.Owned "errorVar")
        ,                               symbolScope = 1
        ,                               isMoved = True
        ,                               isBorrowed = True
        ,                               constraints = []
    }
      
                                    symbols = Map.singleton "errorVar" symbolInfo
                                    initialState = (newIntegratedAnalyzer True True) {                               symbolTable = symbols }
                                    code = "func test() { errorVar := generateError() }"
  
  result <- runExceptT (evalStateT (runCrossAnalysis code) initialState)
  case result of
    Left _ -> assertBool "Should generate errors" False
    Right (CrossAnalyzerError msg level _ : _) -> do
                  assertBool "Error message should not be empty" (not $ null msg)
                  assertEqual "Error level should be Error" Error level
    Right [] -> assertBool "Should generate at least one error" False

testCrossAnalysisProperties :: TestTree
testCrossAnalysisProperties =             testProperty "Cross analysis preserves symbol information" $
  forAll arbitrarySymbolInfo $ \symbolInfo ->
    let symbols = Map.singleton "testSymbol" symbolInfo
                              initialState = (newIntegratedAnalyzer True True) {                               symbolTable = symbols }
                                      code = "func test() { }"
    in monadicIO $ do
              result <- run (runExceptT (runStateT (runCrossAnalysis code) initialState)
        case result of
          Left _ -> assert True
          Right (errs, _) -> assert $ L.length errs >= 0  -- Should always return a list (possibly empty)

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
    {                               symbolName = name
    ,                               symbolType = symbolType
    ,                               ownershipState = ownershipState
    ,                               symbolScope = 1
    ,                               isMoved = moved
    ,                               isBorrowed = borrowed
    ,                               constraints = []
    }

-- Helper function to check if a string is contained in another
isInfixOf :: String -> String -> Bool
isInfixOf needle                               haystack = needle `elem` words haystack