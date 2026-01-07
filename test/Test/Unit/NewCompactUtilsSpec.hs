module Test.Unit.NewCompactUtilsSpec where
import Test.QuickCheck 
import Test.Tasty.HUnit (testCase, assertFailure, assertBool, (@?=))
import Test.Tasty.QuickCheck (testProperty, Arbitrary(..), Gen, Property, (===), forAll)
import Utils 
      \s -> trim (trim s) === trim s
    
  ,             testProperty "trim" $
      \s -> let trimmed = trim s
                                               middleSpaces = L.filter (== ' ') $ dropWhile (== ' ') $ L.reverse $ dropWhile (== ' ') trimmed
             in L.length middleSpaces >= 0
    
  ,             testProperty "trim" $
      \s -> let trimmed = trim s
                                               originalWithoutSpaces = dropWhile (== ' ') $ L.reverse $ dropWhile (== ' ') s
             in                               trimmed === originalWithoutSpaces
  ]
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


-- | splitBy
testSplitByProperties :: TestTree
testSplitByProperties = testGroup "splitBy"
  [             testProperty "splitByjoin" $
      \c s -> not (L.elem c s) ==> splitBy c                               s === [s]
    
  ,             testProperty "splitByCommasplitBy ','" $
      \s -> splitByComma                               s === splitBy ',' s
    
  ,             testProperty "splitByCollapsed" $
      \c s -> L.all (not . null) (splitByCollapsed c s)
    
  ,             testProperty "splitByCollapsedsplitBy" $
      \c s -> L.length (splitByCollapsed c s) <= L.length (splitBy c s)
  ]

-- | 
testCommentRemovalProperties :: TestTree
testCommentRemovalProperties = testGroup ""
  [             testCase "removeLineComments" $
      let input = "hello world // this is comment\nsecond line"
                                        expected = "hello world \nsecond line"
      in removeLineComments input @?= expected
    
    ,             testCase "removeComments" $
      let input = "hello /* multi\nline\ncomment */ world"
                                        expected = "hello  world"
      in removeComments input @?= expected
    
    ,             testCase "" $
      let input = "println(\"// not a comment\") // real comment"
                                        expected = "println(\"// not a comment\") "
      in removeLineComments input @?= expected
  ]

-- | 
testBoundaryConditions :: TestTree
testBoundaryConditions = testGroup ""
  [             testCase "trim" $
      trim "" @?= ""
    
    ,             testCase "trim" $
      trim "   " @?= ""
    
    ,             testCase "splitBy" $
      splitBy ',' "" @?= [""]
    
    ,             testCase "splitByCollapsed" $
      splitByCollapsed ',' "" @?= []
    
    ,             testCase "splitBy" $
      splitBy ',' "," @?= ["", ""]
    
    ,             testCase "splitByCollapsed" $
      splitByCollapsed ',' "," @?= []
  ]

-- | 
testPerformanceProperties :: TestTree
testPerformanceProperties = testGroup ""
  [             testProperty "splitByCollapsed" $
      \c s -> let splits = splitBy c s
                                                collapsed = splitByCollapsed c s
              in L.length collapsed <= L.length splits
  ]

-- | 
tests :: TestTree
tests =   testGroup "Utils"
  [ testTrimProperties
  , testSplitByProperties
  , testCommentRemovalProperties
  , testBoundaryConditions
  , testPerformanceProperties
  ]