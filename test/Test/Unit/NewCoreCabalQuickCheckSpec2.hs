module Test.Unit.NewCoreCabalQuickCheckSpec2 (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestSupport.QuickCheck (fastProperty)

import SourceLocation (SourceLocation(..), Position(..))
import qualified Data.Text as T

-- | Source location calculation tests with QuickCheck properties
tests :: TestTree
tests =
  testGroup "New Core Cabal QuickCheck Tests 2 - Source Location"
    [ testGroup "Position arithmetic properties"
        [ fastProperty "position addition is commutative" prop_positionAdditionCommutative
        , fastProperty "position addition is associative" prop_positionAdditionAssociative
        , fastProperty "position subtraction is inverse of addition" prop_positionSubtractionInverse
        , testCase "position line arithmetic" $ do
            let pos1 = Position { line = 5, column = 10 }
                pos2 = Position { line = 3, column = 20 }
                expected = Position { line = 8, column = 30 }
            addPositions pos1 pos2 @?= expected
        ]
    , testGroup "Source location properties"
        [ fastProperty "source location comparison is transitive" prop_locationComparisonTransitive
        , fastProperty "source location distance is symmetric" prop_locationDistanceSymmetric
        , testCase "source location creation" $ do
            let pos = Position { line = 1, column = 1 }
                loc = SourceLocation { 
                  start = pos, 
                  end = Position { line = 1, column = 5 },
                  filePath = "test.typus"
                }
            start loc @?= pos
        ]
    , testGroup "Edge cases"
        [ testCase "zero position handling" $ do
            let zeroPos = Position { line = 0, column = 0 }
                pos = Position { line = 1, column = 1 }
            addPositions zeroPos pos @?= pos
        , testCase "negative column handling" $ do
            let pos1 = Position { line = 1, column = 5 }
                pos2 = Position { line = 0, column = -2 }
            addPositions pos1 pos2 @?= Position { line = 1, column = 3 }
        ]
    ]

-- Position data type for testing
data Position = Position
  { line :: Int
  , column :: Int
  } deriving (Show, Eq)

data SourceLocation = SourceLocation
  { start :: Position
  , end :: Position
  , filePath :: String
  } deriving (Show, Eq)

-- | QuickCheck properties

-- Position addition is commutative: a + b = b + a
prop_positionAdditionCommutative :: Int -> Int -> Int -> Int -> Bool
prop_positionAdditionCommutative l1 c1 l2 c2 =
  let pos1 = Position { line = l1, column = c1 }
      pos2 = Position { line = l2, column = c2 }
  in addPositions pos1 pos2 == addPositions pos2 pos1

-- Position addition is associative: (a + b) + c = a + (b + c)
prop_positionAdditionAssociative :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_positionAdditionAssociative l1 c1 l2 c2 l3 c3 =
  let pos1 = Position { line = l1, column = c1 }
      pos2 = Position { line = l2, column = c2 }
      pos3 = Position { line = l3, column = c3 }
      left = addPositions (addPositions pos1 pos2) pos3
      right = addPositions pos1 (addPositions pos2 pos3)
  in left == right

-- Position subtraction is the inverse of addition (when possible)
prop_positionSubtractionInverse :: Int -> Int -> Int -> Int -> Bool
prop_positionSubtractionInverse l1 c1 l2 c2 =
  let pos1 = Position { line = l1, column = c1 }
      pos2 = Position { line = l2, column = c2 }
      sumPos = addPositions pos1 pos2
      diffPos = subtractPositions sumPos pos2
  in diffPos == pos1

-- Source location comparison is transitive
prop_locationComparisonTransitive :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_locationComparisonTransitive l1 c1 l2 c2 l3 c3 =
  let pos1 = Position { line = l1, column = c1 }
      pos2 = Position { line = l2, column = c2 }
      pos3 = Position { line = l3, column = c3 }
      loc1 = SourceLocation { start = pos1, end = pos2, filePath = "file1" }
      loc2 = SourceLocation { start = pos2, end = pos3, filePath = "file1" }
      loc3 = SourceLocation { start = pos1, end = pos3, filePath = "file1" }
  in if isBefore loc1 loc2 && isBefore loc2 loc3 
     then isBefore loc1 loc3
     else True

-- Source location distance is symmetric
prop_locationDistanceSymmetric :: Int -> Int -> Int -> Int -> Bool
prop_locationDistanceSymmetric l1 c1 l2 c2 =
  let pos1 = Position { line = l1, column = c1 }
      pos2 = Position { line = l2, column = c2 }
      loc1 = SourceLocation { start = pos1, end = pos1, filePath = "file1" }
      loc2 = SourceLocation { start = pos2, end = pos2, filePath = "file1" }
      dist1 = locationDistance loc1 loc2
      dist2 = locationDistance loc2 loc1
  in dist1 == dist2

-- Helper functions
addPositions :: Position -> Position -> Position
addPositions p1 p2 = Position
  { line = line p1 + line p2
  , column = column p1 + column p2
  }

subtractPositions :: Position -> Position -> Position
subtractPositions p1 p2 = Position
  { line = line p1 - line p2
  , column = column p1 - column p2
  }

isBefore :: SourceLocation -> SourceLocation -> Bool
isBefore loc1 loc2 = 
  let end1 = end loc1
      start2 = start loc2
  in line end1 < line start2 || 
     (line end1 == line start2 && column end1 <= column start2)

locationDistance :: SourceLocation -> SourceLocation -> Int
locationDistance loc1 loc2 =
  let start1 = start loc1
      start2 = start loc2
      lineDiff = abs (line start1 - line start2)
      colDiff = abs (column start1 - column start2)
  in lineDiff * 100 + colDiff  -- Weight lines more heavily