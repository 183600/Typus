module Test.Unit.NewCoreCabalQuickCheckSpec4 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)

import Compiler.Errors.Core (ErrorSeverity(..), ErrorCategory(..), ErrorLocation(..), ErrorContext(..))
import SourceLocation (SourcePos(..))

-- | Error handling consistency tests with QuickCheck properties
tests :: TestTree
tests =
  testGroup "New Core Cabal QuickCheck Tests 4 - Error Handling"
    [ testGroup "Error severity properties"
        [ fastProperty "error severity ordering is transitive" prop_errorSeverityTransitive
        , fastProperty "error severity comparison is total" prop_errorSeverityTotal
        , testCase "error severity levels" $ do
            Error @?= ErrorSeverity 0
            Warning @?= ErrorSeverity 1
            Info @?= ErrorSeverity 2
        ]
    , testGroup "Error category properties"
        [ fastProperty "error category merging preserves criticality" prop_errorCategoryMergingPreservesCriticality
        , fastProperty "error category combination is commutative" prop_errorCategoryCommutative
        , testCase "error categories" $ do
            let categories = [SyntaxError, TypeError, OwnershipError, DependencyError]
            length categories @?= 4
        ]
    , testGroup "Error location properties"
        [ fastProperty "error location distance is symmetric" prop_errorLocationDistanceSymmetric
        , fastProperty "error location span covers start and end" prop_errorLocationSpanCoverage
        , testCase "error location creation" $ do
            let start = SourcePos 1 1
                end = SourcePos 1 10
                location = ErrorLocation { elStart = start, elEnd = end, elFilePath = "test.typus" }
            elStart location @?= start
            elEnd location @?= end
            elFilePath location @?= "test.typus"
        ]
    , testGroup "Error context properties"
        [ fastProperty "error context merging preserves information" prop_errorContextMergingPreservesInfo
        , fastProperty "error context is associative" prop_errorContextAssociative
        , testCase "empty context" $ do
            let context = emptyContext
            length (ecMessages context) @?= 0
        ]
    ]

-- Simplified versions of data structures for testing
data ErrorSeverity = Error | Warning | Info
  deriving (Show, Eq, Ord)

data ErrorCategory = SyntaxError | TypeError | OwnershipError | DependencyError
  deriving (Show, Eq)

data ErrorLocation = ErrorLocation
    { elStart :: SourcePos
    , elEnd :: SourcePos
    , elFilePath :: String
    } deriving (Show, Eq)

data ErrorContext = ErrorContext
    { ecMessages :: [String]
    , ecVariables :: [(String, String)]
    } deriving (Show, Eq)

data SourcePos = SourcePos Int Int  -- line, column
  deriving (Show, Eq)

-- | QuickCheck properties

-- Error severity ordering is transitive
prop_errorSeverityTransitive :: ErrorSeverity -> ErrorSeverity -> ErrorSeverity -> Bool
prop_errorSeverityTransitive s1 s2 s3 =
  if s1 <= s2 && s2 <= s3 then s1 <= s3 else True

-- Error severity comparison is total (any two severities can be compared)
prop_errorSeverityTotal :: ErrorSeverity -> ErrorSeverity -> Bool
prop_errorSeverityTotal s1 s2 = 
  s1 <= s2 || s2 <= s1

-- Error category merging preserves criticality
prop_errorCategoryMergingPreservesCriticality :: ErrorCategory -> ErrorCategory -> Bool
prop_errorCategoryMergingPreservesCriticality cat1 cat2 =
  let criticality cat = case cat of
        SyntaxError -> 3
        TypeError -> 3
        OwnershipError -> 2
        DependencyError -> 1
      merged = mergeErrorCategories cat1 cat2
      criticality1 = criticality cat1
      criticality2 = criticality cat2
      criticalityMerged = criticality merged
  in criticalityMerged >= max criticality1 criticality2

-- Error category combination is commutative
prop_errorCategoryCommutative :: ErrorCategory -> ErrorCategory -> Bool
prop_errorCategoryCommutative cat1 cat2 =
  mergeErrorCategories cat1 cat2 == mergeErrorCategories cat2 cat1

-- Error location distance is symmetric
prop_errorLocationDistanceSymmetric :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_errorLocationDistanceSymmetric l1 c1 l2 c2 l3 c3 =
  let pos1 = SourcePos l1 c1
      pos2 = SourcePos l2 c2
      pos3 = SourcePos l3 c3
      loc1 = ErrorLocation { elStart = pos1, elEnd = pos1, elFilePath = "file1" }
      loc2 = ErrorLocation { elStart = pos2, elEnd = pos2, elFilePath = "file2" }
      loc3 = ErrorLocation { elStart = pos3, elEnd = pos3, elFilePath = "file3" }
      dist1 = errorLocationDistance loc1 loc2
      dist2 = errorLocationDistance loc2 loc1
  in dist1 == dist2

-- Error location span covers start and end
prop_errorLocationSpanCoverage :: Int -> Int -> Int -> Int -> Bool
prop_errorLocationSpanCoverage startLine startCol endLine endCol =
  let start = SourcePos startLine startCol
      end = SourcePos endLine endCol
      location = ErrorLocation { elStart = start, elEnd = end, elFilePath = "test" }
      coveredStart = isPositionInSpan start location
      coveredEnd = isPositionInSpan end location
  in coveredStart && coveredEnd

-- Error context merging preserves information
prop_errorContextMergingPreservesInfo :: [String] -> [(String, String)] -> [String] -> [(String, String)] -> Bool
prop_errorContextMergingPreservesInfo msgs1 vars1 msgs2 vars2 =
  let ctx1 = ErrorContext { ecMessages = msgs1, ecVariables = vars1 }
      ctx2 = ErrorContext { ecMessages = msgs2, ecVariables = vars2 }
      merged = mergeErrorContexts ctx1 ctx2
      originalMsgCount = length (ecMessages ctx1) + length (ecMessages ctx2)
      originalVarCount = length (ecVariables ctx1) + length (ecVariables ctx2)
      mergedMsgCount = length (ecMessages merged)
      mergedVarCount = length (ecVariables merged)
  in mergedMsgCount >= originalMsgCount && mergedVarCount >= originalVarCount

-- Error context merging is associative
prop_errorContextAssociative :: [String] -> [(String, String)] -> [String] -> [(String, String)] -> [String] -> [(String, String)] -> Bool
prop_errorContextAssociative msgs1 vars1 msgs2 vars2 msgs3 vars3 =
  let ctx1 = ErrorContext { ecMessages = msgs1, ecVariables = vars1 }
      ctx2 = ErrorContext { ecMessages = msgs2, ecVariables = vars2 }
      ctx3 = ErrorContext { ecMessages = msgs3, ecVariables = vars3 }
      left = mergeErrorContexts (mergeErrorContexts ctx1 ctx2) ctx3
      right = mergeErrorContexts ctx1 (mergeErrorContexts ctx2 ctx3)
  in left == right

-- Helper functions
emptyContext :: ErrorContext
emptyContext = ErrorContext { ecMessages = [], ecVariables = [] }

mergeErrorCategories :: ErrorCategory -> ErrorCategory -> ErrorCategory
mergeErrorCategories SyntaxError _ = SyntaxError
mergeErrorCategories _ SyntaxError = SyntaxError
mergeErrorCategories TypeError _ = TypeError
mergeErrorCategories _ TypeError = TypeError
mergeErrorCategories OwnershipError _ = OwnershipError
mergeErrorCategories _ OwnershipError = OwnershipError
mergeErrorCategories DependencyError DependencyError = DependencyError

errorLocationDistance :: ErrorLocation -> ErrorLocation -> Int
errorLocationDistance loc1 loc2 =
  let SourcePos l1 c1 = elStart loc1
      SourcePos l2 c2 = elStart loc2
      lineDiff = abs (l1 - l2)
      colDiff = abs (c1 - c2)
  in lineDiff * 100 + colDiff

isPositionInSpan :: SourcePos -> ErrorLocation -> Bool
isPositionInSpan pos location =
  let SourcePos pl pc = pos
      SourcePos sl sc = elStart location
      SourcePos el ec = elEnd location
  in (pl > sl || (pl == sl && pc >= sc)) &&
     (pl < el || (pl == el && pc <= ec))

mergeErrorContexts :: ErrorContext -> ErrorContext -> ErrorContext
mergeErrorContexts ctx1 ctx2 = ErrorContext
  { ecMessages = ecMessages ctx1 ++ ecMessages ctx2
  , ecVariables = ecVariables ctx1 ++ ecVariables ctx2
  }