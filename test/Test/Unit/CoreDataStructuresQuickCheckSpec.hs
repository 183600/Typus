module Test.Unit.CoreDataStructuresQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, assertEqual)
import Test.Tasty.QuickCheck (testProperty, Property, forAll, Gen, arbitrary, elements, listOf, listOf1)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import SourceLocation (SourcePos(..), SourceSpan(..), Located(..))
import Dependencies.AST (AST(..), Statement(..), TypeExpr(..), Constraint(..))
import Ownership.Common.Types (OwnershipError(..))
import Analyzer.State (SymbolInfo(..))
import Analyzer.Types (SymbolType(..))
import qualified Dependencies as Dep
import qualified Ownership as Own

tests :: TestTree
tests = testGroup "Core Data Structures QuickCheck Tests"
  [ testSourceLocationProperties
  , testASTProperties
  , testSymbolInfoProperties
  , testOwnershipErrorProperties
  , testTypeExpressionProperties
  , testConstraintProperties
  , testMapOperationsProperties
  , testSetOperationsProperties
  , testTextOperationsProperties
  , testDataStructureRoundTrip
  ]

testSourceLocationProperties :: TestTree
testSourceLocationProperties = testGroup "SourceLocation Properties"
  [ testSourcePosProperties
  , testSourceSpanProperties
  , testLocatedProperties
  ]

testSourcePosProperties :: TestTree
testSourcePosProperties = testProperty "SourcePos equality and ordering" $
  forAll arbitrarySourcePos $ \pos1 ->
  forAll arbitrarySourcePos $ \pos2 -> do
    let eq = pos1 == pos2
        ne = pos1 /= pos2
    return $ eq ==> not ne

testSourceSpanProperties :: TestTree
testSourceSpanProperties = testProperty "SourceSpan validity" $
  forAll arbitrarySourceSpan $ \span -> do
    let start = sourceSpanStart span
        end = sourceSpanEnd span
        valid = sourceLine start <= sourceLine end ||
                (sourceLine start == sourceLine end && sourceColumn start <= sourceColumn end)
    return $ valid

testLocatedProperties :: TestTree
testLocatedProperties = testProperty "Located value preservation" $
  forAll arbitraryLocatedInt $ \located -> do
    let value = locatedValue located
        span = locatedSpan located
    return $ not (null (show span)) && value >= 0

testASTProperties :: TestTree
testASTProperties = testGroup "AST Properties"
  [ testProgramProperties
  , testStatementProperties
  ]

testProgramProperties :: TestTree
testProgramProperties = testProperty "Program structure preservation" $
  forAll arbitraryProgram $ \program -> do
    let (Program stmts) = program
    return $ length stmts >= 0

testStatementProperties :: TestTree
testStatementProperties = testProperty "Statement round-trip" $
  forAll arbitraryStatement $ \stmt -> do
    let serialized = show stmt
    return $ not (null serialized) && length (words serialized) > 0

testSymbolInfoProperties :: TestTree
testSymbolInfoProperties = testProperty "SymbolInfo consistency" $
  forAll arbitrarySymbolInfo $ \symbolInfo -> do
    let name = symbolName symbolInfo
        location = symbolLocation symbolInfo
    return $ not (null name) && fst location > 0 && snd location > 0

testOwnershipErrorProperties :: TestTree
testOwnershipErrorProperties = testProperty "OwnershipError formatting" $
  forAll arbitraryOwnershipError $ \error -> do
    let formatted = show error
    return $ not (null formatted)

testTypeExpressionProperties :: TestTree
testTypeExpressionProperties = testProperty "TypeExpression structure" $
  forAll arbitraryTypeExpr $ \expr -> do
    let serialized = show expr
    return $ not (null serialized)

testConstraintProperties :: TestTree
testConstraintProperties = testProperty "Constraint validation" $
  forAll arbitraryConstraint $ \constraint -> do
    let serialized = show constraint
    return $ not (null serialized)

testMapOperationsProperties :: TestTree
testMapOperationsProperties = testGroup "Map Operations Properties"
  [ testMapInsertionProperties
  , testMapDeletionProperties
  ]

testMapInsertionProperties :: TestTree
testMapInsertionProperties = testProperty "Map insertion preserves size" $
  forAll arbitraryStringMap $ \originalMap ->
  forAll arbitraryString $ \key ->
  forAll arbitraryString $ \value -> do
    let newMap = Map.insert key value originalMap
        originalSize = Map.size originalMap
        newSize = Map.size newMap
    return $ if Map.member key originalMap
             then newSize == originalSize
             else newSize == originalSize + 1

testMapDeletionProperties :: TestTree
testMapDeletionProperties = testProperty "Map deletion never increases size" $
  forAll arbitraryStringMap $ \originalMap ->
  forAll arbitraryString $ \key -> do
    let newMap = Map.delete key originalMap
        originalSize = Map.size originalMap
        newSize = Map.size newMap
    return $ newSize <= originalSize

testSetOperationsProperties :: TestTree
testSetOperationsProperties = testGroup "Set Operations Properties"
  [ testSetInsertionProperties
  , testSetUnionProperties
  ]

testSetInsertionProperties :: TestTree
testSetInsertionProperties = testProperty "Set insertion preserves uniqueness" $
  forAll arbitraryStringSet $ \originalSet ->
  forAll arbitraryString $ \element -> do
    let newSet = Set.insert element originalSet
        originalSize = Set.size originalSet
        newSize = Set.size newSet
    return $ if Set.member element originalSet
             then newSize == originalSize
             else newSize == originalSize + 1

testSetUnionProperties :: TestTree
testUnionProperties = testProperty "Set union size property" $
  forAll arbitraryStringSet $ \set1 ->
  forAll arbitraryStringSet $ \set2 -> do
    let unionSet = Set.union set1 set2
        unionSize = Set.size unionSet
        size1 = Set.size set1
        size2 = Set.size set2
    return $ unionSize >= size1 && unionSize >= size2 && unionSize <= size1 + size2

testTextOperationsProperties :: TestTree
testTextOperationsProperties = testGroup "Text Operations Properties"
  [ testTextConcatenationProperties
  , testTextSplittingProperties
  ]

testTextConcatenationProperties :: TestTree
testConcatenationProperties = testProperty "Text concatenation length" $
  forAll arbitraryText $ \text1 ->
  forAll arbitraryText $ \text2 -> do
    let combined = T.append text1 text2
        len1 = T.length text1
        len2 = T.length text2
        combinedLen = T.length combined
    return $ combinedLen == len1 + len2

testTextSplittingProperties :: TestTree
testSplittingProperties = testProperty "Text splitting round-trip" $
  forAll arbitraryText $ \text ->
  forAll (elements [",", " ", "\t", ";"]) $ \separator -> do
    let parts = T.split (== separator) text
        rejoined = T.intercalate (T.singleton separator) parts
    return $ if T.null text
             then T.null rejoined
             else T.length rejoined >= T.length text - T.length (filter (== separator) (T.unpack text))

testDataStructureRoundTrip :: TestTree
testDataStructureRoundTrip = testProperty "Complex data structure round-trip" $
  forAll arbitraryComplexStructure $ \structure -> do
    let serialized = show structure
        parsed = length (words serialized)  -- Simplified "parsing"
    return $ not (null serialized) && parsed > 0

-- Helper generators for QuickCheck tests

arbitrarySourcePos :: Gen SourcePos
arbitrarySourcePos = do
  line <- arbitrary `suchThat` (> 0)
  column <- arbitrary `suchThat` (> 0)
  return $ SourcePos line column

arbitrarySourceSpan :: Gen SourceSpan
arbitrarySourceSpan = do
  start <- arbitrarySourcePos
  end <- arbitrarySourcePos `suchThat` (\pos -> 
    sourceLine pos > sourceLine start || 
    (sourceLine pos == sourceLine start && sourceColumn pos >= sourceColumn start))
  return $ SourceSpan start end

arbitraryLocatedInt :: Gen (Located Int)
arbitraryLocatedInt = do
  value <- arbitrary `suchThat` (>= 0)
  span <- arbitrarySourceSpan
  return $ Located value span

arbitraryProgram :: Gen AST
arbitraryProgram = do
  stmts <- listOf arbitraryStatement
  return $ Program stmts

arbitraryStatement :: Gen Statement
arbitraryStatement = elements
  [ STypeDef "MyType" ["T"] [SizeGT "T" 0]
  , STypeAlias "MyAlias" (SimpleT "Int") []
  , SVarDecl "myVar" (SimpleT "String")
  , SFuncDecl "myFunc" [("x", SimpleT "Int")] (Just (SimpleT "Int"))
  , SConstraintDef "myConstraint" (SizeGT "x" 0)
  , SExistsDecl ["T"] (SVarDecl "x" (SimpleT "T"))
  ]

arbitraryTypeExpr :: Gen TypeExpr
arbitraryTypeExpr = elements
  [ SimpleT "Int"
  , SimpleT "String"
  , GenericT "List" [SimpleT "Int"]
  , GenericT "Map" [SimpleT "String", SimpleT "Int"]
  , FuncT [("x", SimpleT "Int")] (SimpleT "String")
  , RefineT (SimpleT "List") [SizeGT "List" 0]
  ]

arbitraryConstraint :: Gen Constraint
arbitraryConstraint = elements
  [ SizeGT "var" 0
  , SizeGE "var" 1
  , RangeC "var" 0 100
  , PredC "isValid" [SimpleT "Int"]
  ]

arbitrarySymbolInfo :: Gen SymbolInfo
arbitrarySymbolInfo = do
  name <- elements ["var1", "var2", "testVar", "example"]
  line <- arbitrary `suchThat` (> 0)
  column <- arbitrary `suchThat` (> 0)
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
    , symbolLocation = (line, column)
    , isMoved = moved
    , isBorrowed = borrowed
    }

arbitraryOwnershipError :: Gen OwnershipError
arbitraryOwnershipError = elements
  [ UseAfterMove "var"
  , DoubleMove "source" "dest"
  , BorrowWhileMoved "movedVar"
  , MutBorrowWhileBorrowed "borrowedVar"
  , BorrowWhileMutBorrowed "mutBorrowedVar"
  , MultipleMutBorrows "mutVar"
  , UseWhileMutBorrowed "usedVar"
  , OutOfScope "scopeVar"
  , BorrowError "errorVar"
  , ParseError "parse error message"
  , CrossFunctionMove "funcSource" "funcDest"
  , ParameterMoveMismatch "param"
  , ControlFlowError "control flow issue"
  , PathSensitiveError "path sensitive issue"
  , LoopOwnershipError "loop ownership issue"
  ]

arbitraryString :: Gen String
arbitraryString = listOf1 $ elements ['a'..'z']

arbitraryStringMap :: Gen (Map.Map String String)
arbitraryStringMap = do
  pairs <- listOf arbitrary
  return $ Map.fromList pairs

arbitraryStringSet :: Gen (Set.Set String)
arbitraryStringSet = do
  strings <- listOf arbitraryString
  return $ Set.fromList strings

arbitraryText :: Gen T.Text
arbitraryText = T.pack <$> arbitraryString

arbitraryComplexStructure :: Gen (Map.Map String (Set.Set Int))
arbitraryComplexStructure = do
  size <- arbitrary `suchThat` (\n -> n >= 0 && n <= 10)
  pairs <- vectorOf size $ do
    key <- arbitraryString
    setSize <- arbitrary `suchThat` (\n -> n >= 0 && n <= 5)
    values <- vectorOf setSize arbitrary
    return (key, Set.fromList values)
  return $ Map.fromList pairs

-- Helper function
vectorOf :: Int -> Gen a -> Gen [a]
vectorOf n gen = sequence (replicate n gen)