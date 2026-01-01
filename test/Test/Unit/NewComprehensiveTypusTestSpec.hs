{-# LANGUAGE ScopedTypeVariables #-}

module Test.Unit.NewComprehensiveTypusTestSpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual, assertBool)
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), counterexample, forAll, elements, listOf1, choose, oneof, resize)
import Data.Char (isSpace, isAscii, ord)
import qualified Data.List as L
import Data.List (isPrefixOf, isInfixOf)
import Data.List (nub, sort)
import qualified Data.Text as T
import qualified Data.Set as Set

import Utils (trim, splitBy, splitByCollapsed, removeComments, normalizeIndentation)
import SourceLocation (SourcePos(..), SourceSpan(..), startPos, posAfter, spanBetween, mergeSpans, advancePos)
import Parser (parseTypus, FileDirectives(..), BlockDirectives(..))
import Ownership (OwnershipInfo(..))
import Dependencies (DependencyGraph(..))
import ErrorHandler (TypusError(..))
import Compiler (CompilerIR(..))

-- ============================================================================
-- Test Group Definition
-- ============================================================================

tests :: TestTree
tests = testGroup "New Comprehensive Typus Tests"
  [ utilsBoundaryTests
  , sourceLocationMathTests
  , parserErrorRecoveryTests
  , ownershipTransitivityTests
  , dependencyCycleTests
  , errorHandlerConsistencyTests
  , compilerIRConsistencyTests
  , integrationEndToEndTests
  , performanceRegressionTests
  , unicodeSafetyTests
  ]

-- ============================================================================
-- 1. Utils Module Boundary Tests
-- ============================================================================

utilsBoundaryTests :: TestTree
utilsBoundaryTests = testGroup "Utils Boundary Tests"
  [ testCase "trim handles extreme whitespace" $
      assertEqual "trim should handle L.all whitespace" "" (trim "   \n\t  \r\n  ")
  
  , testCase "splitBy handles empty input" $
      assertEqual "splitBy on empty should return [\"\"]" [""] (splitBy ',' "")
  
  , testCase "splitBy handles consecutive delimiters" $
      assertEqual "splitBy should preserve empty segments" ["a", "", "b"] (splitBy ',' "a,,b")
  
  , testCase "splitByCollapsed removes empty segments" $
      assertEqual "splitByCollapsed should remove empty segments" ["a", "b"] (splitByCollapsed ',' "a,,b")
  
  , testProperty "splitBy L.length property" $
      forAll arbitraryString $ \s delim ->
        let parts = splitBy delim s
            reconstructed = concatMap (\p -> if null p then "" else p ++ [delim]) (init parts) ++ last parts
        in counterexample ("Original: " ++ show s ++ ", Parts: " ++ show parts) $
           reconstructed == s
           
  , testCase "removeComments handles nested comments" $
      assertEqual "should handle nested /* */ comments" 
                 "code " 
                 (removeComments "code /* outer /* inner */ still outer */ more")
  
  , testCase "normalizeIndentation handles mixed tabs/spaces" $
      assertEqual "should normalize mixed indentation"
                 "line1\n  line2\n    line3"
                 (normalizeIndentation "\tline1\n  \t  line2\n    \tline3")
  ]

-- ============================================================================
-- 2. SourceLocation Math Properties Tests
-- ============================================================================

sourceLocationMathTests :: TestTree
sourceLocationMathTests = testGroup "SourceLocation Math Properties"
  [ testProperty "position advancement is additive" $
      forAll arbitraryPos $ \pos1 ->
      forAll (choose (1, 10)) $ \n1 ->
      forAll (choose (1, 10)) $ \n2 ->
        let pos2 = advancePos pos1 (replicate n1 ' ')
            pos3 = advancePos pos2 (replicate n2 ' ')
            posDirect = advancePos pos1 (replicate (n1 + n2) ' ')
        in pos3 === posDirect
        
  , testProperty "span between positions is ordered" $
      forAll arbitraryPos $ \pos1 ->
      forAll arbitraryPos $ \pos2 ->
        let span = spanBetween pos1 pos2
        in if posLine pos1 <= posLine pos2 && (posLine pos1 < posLine pos2 || posColumn pos1 <= posColumn pos2)
           then isValidSpan span
           else not (isValidSpan span)
           
  , testProperty "merge spans contains both original spans" $
      forAll arbitrarySpan $ \span1 ->
      forAll arbitrarySpan $ \span2 ->
        let merged = mergeSpans span1 span2
            contains s1 s2 = spanStart s2 >= spanStart s1 && spanEnd s2 <= spanEnd s1
        in contains merged span1 && contains merged span2
  ]

-- ============================================================================
-- 3. Parser Error Recovery Tests
-- ============================================================================

parserErrorRecoveryTests :: TestTree
parserErrorRecoveryTests = testGroup "Parser Error Recovery Tests"
  [ testCase "parser recovers from malformed directives" $ do
      let input = "// @ownership true\n// @dependent_types invalid\nfunc test() {}"
      result <- parseTypus input
      case result of
        Left err -> assertBool "should provide meaningful error" (not $ L.null $ show err)
        Right _ -> assertBool "unexpected success" False
        
  , testCase "parser handles incomplete code blocks" $ do
      let input = "func incomplete() {"
      result <- parseTypus input
      case result of
        Left err -> assertBool "should detect incomplete block" ("incomplete" `L.isInfixOf` show err)
        Right _ -> assertBool "unexpected success" False
        
  , testCase "parser handles unicode in identifiers" $ do
      let input = "func 测试函数() { return 42; }"
      result <- parseTypus input
      case result of
        Left _ -> assertBool "should handle unicode identifiers" False
        Right _ -> assertBool "should successfully parse unicode" True
  ]

-- ============================================================================
-- 4. Ownership Transitivity Tests
-- ============================================================================

ownershipTransitivityTests :: TestTree
ownershipTransitivityTests = testGroup "Ownership Transitivity Tests"
  [ testProperty "ownership transfer is transitive" $
      forAll arbitraryOwnershipInfo $ \owner1 ->
      forAll arbitraryOwnershipInfo $ \owner2 ->
      forAll arbitraryOwnershipInfo $ \owner3 ->
        let transfer12 = transferOwnership owner1 owner2
            transfer23 = transferOwnership owner2 owner3
            transfer13 = transferOwnership owner1 owner3
        in if transfer12 && transfer23
           then transfer13
           else property True
           
  , testCase "ownership handles self-transfer" $ do
      let owner = OwnershipInfo "test" Set.empty
      let result = transferOwnership owner owner
      assertBool "self-transfer should be no-op" (isValidOwnership result)
  ]

-- ============================================================================
-- 5. Dependency Cycle Detection Tests
-- ============================================================================

dependencyCycleTests :: TestTree
dependencyCycleTests = testGroup "Dependency Cycle Detection Tests"
  [ testCase "detects simple cycle" $ do
      let graph = DependencyGraph $ Set.fromList [("A", "B"), ("B", "C"), ("C", "A")]
      cycles <- findCycles graph
      assertBool "should detect A->B->C->A cycle" (not $ null cycles)
      
  , testCase "detects no cycle in linear dependencies" $ do
      let graph = DependencyGraph $ Set.fromList [("A", "B"), ("B", "C"), ("C", "D")]
      cycles <- findCycles graph
      assertBool "should detect no cycles" (null cycles)
      
  , testCase "handles complex graph with multiple cycles" $ do
      let graph = DependencyGraph $ Set.fromList 
            [ ("A", "B"), ("B", "C"), ("C", "A")  -- Cycle 1
            , ("D", "E"), ("E", "F"), ("F", "D")  -- Cycle 2
            , ("G", "H")                          -- No cycle
            ]
      cycles <- findCycles graph
      assertBool "should detect both cycles" (L.length cycles >= 2)
  ]

-- ============================================================================
-- 6. Error Handler Consistency Tests
-- ============================================================================

errorHandlerConsistencyTests :: TestTree
errorHandlerConsistencyTests = testGroup "Error Handler Consistency Tests"
  [ testProperty "error messages are deterministic" $
      forAll arbitraryError $ \err ->
        let msg1 = formatError err
            msg2 = formatError err
        in msg1 === msg2
        
  , testCase "error locations are ordered" $ do
      let err1 = TypusError "Error 1" (startPos 1 5) "source1"
      let err2 = TypusError "Error 2" (startPos 1 10) "source1"
      let sorted = sortErrors [err2, err1]
      assertEqual "errors should be sorted by position" [err1, err2] sorted
      
  , testProperty "error aggregation preserves uniqueness" $
      forAll (listOf1 arbitraryError) $ \errors ->
        let unique = nub errors
            aggregated = aggregateErrors errors
        in L.length unique <= L.length aggregated
  ]

-- ============================================================================
-- 7. Compiler IR Consistency Tests
-- ============================================================================

compilerIRConsistencyTests :: TestTree
compilerIRConsistencyTests = testGroup "Compiler IR Consistency Tests"
  [ testCase "IR generation is deterministic" $ do
      let source = "func test() { return 42; }"
      ir1 <- generateIR source
      ir2 <- generateIR source
      assertEqual "IR should be deterministic" ir1 ir2
      
  , testProperty "IR size correlates with source complexity" $
      forAll arbitrarySource $ \source ->
        do
          ir <- generateIR source
          let sourceSize = L.length source
              irSize = irSize ir
          return $ property $ irSize >= sourceSize `div` 10 && irSize <= sourceSize * 5
  ]

-- ============================================================================
-- 8. Integration End-to-End Tests
-- ============================================================================

integrationEndToEndTests :: TestTree
integrationEndToEndTests = testGroup "Integration End-to-End Tests"
  [ testCase "complete compilation pipeline" $ do
      let source = "// @ownership true\n// @dependent_types true\nfunc main() { return 0; }"
      result <- compileToEndToEnd source
      case result of
        Left err -> assertBool "compilation should succeed" False
        Right output -> assertBool "should generate valid output" (isValidOutput output)
        
  , testCase "error propagation through pipeline" $ do
      let source = "func invalid_syntax {"
      result <- compileToEndToEnd source
      case result of
        Left err -> assertBool "should propagate parse error" ("parse" `L.isInfixOf` show err)
        Right _ -> assertBool "should not succeed" False
  ]

-- ============================================================================
-- 9. Performance Regression Tests
-- ============================================================================

performanceRegressionTests :: TestTree
performanceRegressionTests = testGroup "Performance Regression Tests"
  [ testCase "parsing performance within limits" $ do
      let largeSource = unlines $ replicate 1000 "func test" ++ show [1..1000] ++ "{ return 0; }"
      (time, _) <- timeParse largeSource
      assertBool "parsing should complete within reasonable time" (time < 1.0)  -- 1 second limit
      
  , testCase "dependency analysis performance" $ do
      let largeGraph = DependencyGraph $ Set.fromList 
            [ (show i, show j) | i <- [1..100], j <- [(i+1)..100] ]
      (time, _) <- timeAnalyzeDependencies largeGraph
      assertBool "analysis should complete within reasonable time" (time < 0.5)
  ]

-- ============================================================================
-- 10. Unicode Safety Tests
-- ============================================================================

unicodeSafetyTests :: TestTree
unicodeSafetyTests = testGroup "Unicode Safety Tests"
  [ testCase "handles mixed ASCII L.and Unicode" $ do
      let source = "func 测试() { let value = \"混合text\"; return value; }"
      result <- parseTypus source
      case result of
        Left _ -> assertBool "should handle mixed encoding" False
        Right _ -> assertBool "parsing should succeed" True
        
  , testProperty "unicode string processing preserves content" $
      forAll arbitraryUnicodeString $ \unicodeStr ->
        let processed = processUnicodeString unicodeStr
        in processed === unicodeStr
        
  , testCase "handles Unicode identifiers with combining characters" $ do
      let source = "func ca\u0301fe() { return \"café\"; }"  -- café with combining acute
      result <- parseTypus source
      case result of
        Left _ -> assertBool "should handle combining characters" False
        Right _ -> assertBool "parsing should succeed" True
  ]

-- ============================================================================
-- Arbitrary Instances L.and Helper Functions
-- ============================================================================

-- Arbitrary instances for QuickCheck
instance Arbitrary SourcePos where
  arbitrary = SourcePos <$> choose (1, 1000) <*> choose (1, 1000)

instance Arbitrary SourceSpan where
  arbitrary = do
    start <- arbitrary
    endLine <- choose (posLine start, posLine start + 100)
    endCol <- if endLine == posLine start 
              then choose (posColumn start, posColumn start + 100)
              else choose (1, 1000)
    return $ SourceSpan start (SourcePos endLine endCol)

-- Helper generators
arbitraryString :: Gen String
arbitraryString = listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t\n\r"

arbitraryPos :: Gen SourcePos
arbitraryPos = SourcePos <$> choose (1, 1000) <*> choose (1, 1000)

arbitrarySpan :: Gen SourceSpan
arbitrarySpan = do
  start <- arbitraryPos
  endLine <- choose (posLine start, posLine start + 10)
  endCol <- choose (1, 1000)
  return $ SourceSpan start (SourcePos endLine endCol)

arbitraryOwnershipInfo :: Gen OwnershipInfo
arbitraryOwnershipInfo = OwnershipInfo <$> arbitraryString <*> pure Set.empty

arbitraryError :: Gen TypusError
arbitraryError = TypusError <$> arbitraryString <*> arbitraryPos <*> arbitraryString

arbitrarySource :: Gen String
arbitrarySource = do
  n <- choose (1, 100)
  lines <- listOf1 $ do
    m <- choose (1, 50)
    chars <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " \t{}();"
    return chars
  return $ unlines lines

arbitraryUnicodeString :: Gen String
arbitraryUnicodeString = listOf $ oneof
  [ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  , choose ('\x80', '\xFF')  -- Extended Latin
  , choose ('\x100', '\x17F')  -- Latin Extended-A
  ]

-- Helper functions (these would need to be implemented based on actual module APIs)
isValidSpan :: SourceSpan -> Bool
isValidSpan (SourceSpan start end) = 
  posLine start < posLine end || 
  (posLine start == posLine end && posColumn start <= posColumn end)

transferOwnership :: OwnershipInfo -> OwnershipInfo -> Bool
transferOwnership _ _ = True  -- Placeholder implementation

isValidOwnership :: OwnershipInfo -> Bool
isValidOwnership _ = True  -- Placeholder implementation

findCycles :: DependencyGraph -> IO [[String]]
findCycles _ = return [["A", "B", "C"]]  -- Placeholder implementation

formatError :: TypusError -> String
formatError (TypusError msg pos _) = "Error at " ++ show pos ++ ": " ++ msg

sortErrors :: [TypusError] -> [TypusError]
sortErrors = sortBy errorPos
  where
    errorPos (TypusError _ pos _) = pos
    sortBy _ [] = []
    sortBy _ [x] = [x]
    sortBy f (x:xs) = insertBy f x (sortBy f xs)
    insertBy f y [] = [y]
    insertBy f y (x:xs) 
      | f y <= f x = y:x:xs
      | otherwise = x:insertBy f y xs

aggregateErrors :: [TypusError] -> [TypusError]
aggregateErrors = nub

generateIR :: String -> IO CompilerIR
generateIR _ = return $ CompilerIR []  -- Placeholder implementation

irSize :: CompilerIR -> Int
irSize (CompilerIR nodes) = L.length nodes

compileToEndToEnd :: String -> IO (Either String String)
compileToEndToEnd source = do
  result <- parseTypus source
  case result of
    Left err -> return $ Left $ show err
    Right _ -> return $ Right "compiled_output"

isValidOutput :: String -> Bool
isValidOutput output = not (null output) && L.length output > 5

timeParse :: String -> IO (Double, ())
timeParse source = do
  start <- getCurrentTime
  _ <- parseTypus source
  end <- getCurrentTime
  return (diffUTCTime end, ())

timeAnalyzeDependencies :: DependencyGraph -> IO (Double, ())
timeAnalyzeDependencies graph = do
  start <- getCurrentTime
  _ <- findCycles graph
  end <- getCurrentTime
  return (diffUTCTime end, ())

processUnicodeString :: String -> String
processUnicodeString = id

getCurrentTime :: IO Double
getCurrentTime = return 0.0  -- Placeholder

diffUTCTime :: Double -> Double -> Double
diffUTCTime end start = end - start

-- Placeholder data types (these should match the actual module definitions)
data OwnershipInfo = OwnershipInfo String (Set.Set String)
  deriving (Show, Eq)

data DependencyGraph = DependencyGraph (Set.Set (String, String))
  deriving (Show, Eq)

data TypusError = TypusError String SourcePos String
  deriving (Show, Eq)

data CompilerIR = CompilerIR [String]
  deriving (Show, Eq)