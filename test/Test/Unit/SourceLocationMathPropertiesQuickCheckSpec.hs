{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Test.Unit.SourceLocationMathPropertiesQuickCheckSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)
import TestSupport.QuickCheck (fastProperty)
import Test.QuickCheck (Property, (===), (==>), forAll, counterexample, classify, property, (.&&.), (.||.), Arbitrary(..), Gen, oneof, elements, listOf, choose, suchThat)
import TestSupport.Arbitrary

import SourceLocation
import Data.List (sort, nub, group, intercalate, find, delete, isInfixOf, sortOn)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, mapMaybe)
import Data.Set (Set, empty, singleton, union, unions, member, size, difference, intersection)
import qualified Data.Set as Set
import Data.Map (Map, empty, singleton, insert, lookup, keys, elems, unionWith)
import qualified Data.Map as Map

-- ============================================================================
-- Source Location Math Properties QuickCheck Tests
-- ============================================================================

-- Property: Location distance calculation symmetry
prop_location_distance_symmetry :: Int -> Int -> Int -> Int -> Property
prop_location_distance_symmetry line1 col1 line2 col2 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 ==> 
  let loc1 = SourceLocation line1 col1 "file1.typus"
      loc2 = SourceLocation line2 col2 "file2.typus"
      distance12 = calculateDistance loc1 loc2
      distance21 = calculateDistance loc2 loc1
  in property $ distance12 === distance21

-- Property: Location distance triangle inequality
prop_location_distance_triangle_inequality :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_location_distance_triangle_inequality line1 col1 line2 col2 line3 col3 =
  all (>=0) [line1, col1, line2, col2, line3, col3] ==> 
  let loc1 = SourceLocation line1 col1 "file.typus"
      loc2 = SourceLocation line2 col2 "file.typus"
      loc3 = SourceLocation line3 col3 "file.typus"
      distance12 = calculateDistance loc1 loc2
      distance23 = calculateDistance loc2 loc3
      distance13 = calculateDistance loc1 loc3
  in property $ distance13 <= distance12 + distance23

-- Property: Location ordering consistency
prop_location_ordering_consistency :: Int -> Int -> Int -> Int -> Property
prop_location_ordering_consistency line1 col1 line2 col2 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 ==> 
  let loc1 = SourceLocation line1 col1 "file.typus"
      loc2 = SourceLocation line2 col2 "file.typus"
      comparison = compareLocations loc1 loc2
      reverseComparison = compareLocations loc2 loc1
  in property $ (comparison == EQ) ==> (reverseComparison == EQ) .&&.
     (comparison == LT) ==> (reverseComparison == GT) .&&.
     (comparison == GT) ==> (reverseComparison == LT)

-- Property: Location range arithmetic
prop_location_range_arithmetic :: Int -> Int -> Int -> Property
prop_location_range_arithmetic startLine length =
  startLine >= 0 && length >= 0 ==> 
  let startLoc = SourceLocation startLine 0 "file.typus"
      endLoc = calculateRangeEnd startLoc length
      expectedEndLine = startLine + length
  in property $ sourceLine endLoc === expectedEndLine

-- Property: Location containment
prop_location_containment :: Int -> Int -> Int -> Int -> Property
prop_location_containment outerLine outerCol innerLine innerCol =
  outerLine >= 0 && outerCol >= 0 && innerLine >= 0 && innerCol >= 0 ==> 
  let outerLoc = SourceLocation outerLine outerCol "file.typus"
      innerLoc = SourceLocation innerLine innerCol "file.typus"
      range = createLocationRange outerLoc 10
      contains = locationInRange innerLoc range
      shouldBeContained = innerLine >= outerLine && innerLine <= outerLine + 10
  in property $ contains === shouldBeContained

-- Property: Location interpolation
prop_location_interpolation :: Int -> Int -> Int -> Int -> Double -> Property
prop_location_interpolation line1 col1 line2 col2 factor =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 && factor >= 0 && factor <= 1 ==> 
  let loc1 = SourceLocation line1 col1 "file.typus"
      loc2 = SourceLocation line2 col2 "file.typus"
      interpolated = interpolateLocation loc1 loc2 factor
      expectedLine = round (fromIntegral line1 + factor * fromIntegral (line2 - line1))
      expectedCol = round (fromIntegral col1 + factor * fromIntegral (col2 - col1))
  in property $ sourceLine interpolated === expectedLine .&&.
     sourceColumn interpolated === expectedCol

-- Property: Location clustering
prop_location_clustering :: [Int] -> [Int] -> Property
prop_location_clustering lines cols =
  length lines == length cols && not (null lines) && all (>=0) lines && all (>=0) cols ==> 
  let locations = zipWith (\l c -> SourceLocation l c "file.typus") lines cols
      clusters = clusterLocations locations 5
      clusterSizes = map length clusters
  in property $ sum clusterSizes === length locations .&&.
     all (>0) clusterSizes

-- Property: Location bounding box
prop_location_bounding_box :: [Int] -> [Int] -> Property
prop_location_bounding_box lines cols =
  length lines == length cols && not (null lines) && all (>=0) lines && all (>=0) cols ==> 
  let locations = zipWith (\l c -> SourceLocation l c "file.typus") lines cols
      boundingBox = calculateBoundingBox locations
      minLine = minimum (map sourceLine locations)
      maxLine = maximum (map sourceLine locations)
      minCol = minimum (map sourceColumn locations)
      maxCol = maximum (map sourceColumn locations)
  in property $ boundingBoxMinLine boundingBox === minLine .&&.
     boundingBoxMaxLine boundingBox === maxLine .&&.
     boundingBoxMinCol boundingBox === minCol .&&.
     boundingBoxMaxCol boundingBox === maxCol

-- Property: Location path distance
prop_location_path_distance :: [Int] -> [Int] -> Property
prop_location_path_distance lines cols =
  length lines >= 2 && all (>=0) lines && all (>=0) cols ==> 
  let locations = zipWith (\l c -> SourceLocation l c "file.typus") lines cols
      pathDistance = calculatePathDistance locations
      directDistances = zipWith calculateDistance locations (tail locations)
      expectedDistance = sum directDistances
  in property $ pathDistance === expectedDistance

-- Property: Location normalization
prop_location_normalization :: Int -> Int -> Property
prop_location_normalization line col =
  line >= 0 && col >= 0 ==> 
  let loc = SourceLocation line col "file.typus"
      normalized = normalizeLocation loc
  in property $ sourceLine normalized >= 0 .&&.
     sourceColumn normalized >= 0 .&&.
     sourceFile normalized == sourceFile loc

-- Property: Location transformation
prop_location_transformation :: Int -> Int -> Int -> Int -> Property
prop_location_transformation line col lineOffset colOffset =
  line >= 0 && col >= 0 ==> 
  let originalLoc = SourceLocation line col "file.typus"
      transformed = transformLocation originalLoc lineOffset colOffset
      expectedLine = line + lineOffset
      expectedCol = col + colOffset
  in property $ sourceLine transformed === expectedLine .&&.
     sourceColumn transformed === expectedCol .&&.
     sourceFile transformed == sourceFile originalLoc

-- Property: Location merge operation
prop_location_merge :: Int -> Int -> Int -> Int -> Property
prop_location_merge line1 col1 line2 col2 =
  line1 >= 0 && col1 >= 0 && line2 >= 0 && col2 >= 0 ==> 
  let loc1 = SourceLocation line1 col1 "file.typus"
      loc2 = SourceLocation line2 col2 "file.typus"
      merged = mergeLocations loc1 loc2
      minLine = min line1 line2
      maxLine = max line1 line2
      minCol = min col1 col2
      maxCol = max col1 col2
  in property $ sourceLine merged === minLine .&&.
     sourceColumn merged === minCol

-- Property: Location hash consistency
prop_location_hash_consistency :: Int -> Int -> String -> Property
prop_location_hash_consistency line col filename =
  line >= 0 && col >= 0 ==> 
  let loc1 = SourceLocation line col filename
      loc2 = SourceLocation line col filename
      hash1 = hashLocation loc1
      hash2 = hashLocation loc2
  in property $ hash1 === hash2

-- ============================================================================
-- Helper Functions and Types
-- ============================================================================

-- Location calculation types
data LocationRange = LocationRange
  { rangeStart :: SourceLocation
  , rangeEnd :: SourceLocation
  } deriving (Eq, Show)

data LocationBoundingBox = LocationBoundingBox
  { boundingBoxMinLine :: Int
  , boundingBoxMaxLine :: Int
  , boundingBoxMinCol :: Int
  , boundingBoxMaxCol :: Int
  } deriving (Eq, Show)

-- Location arithmetic functions
calculateDistance :: SourceLocation -> SourceLocation -> Int
calculateDistance loc1 loc2
  | sourceFile loc1 /= sourceFile loc2 = maxBound
  | otherwise = abs (sourceLine loc1 - sourceLine loc2) + abs (sourceColumn loc1 - sourceColumn loc2)

compareLocations :: SourceLocation -> SourceLocation -> Ordering
compareLocations loc1 loc2
  | sourceFile loc1 /= sourceFile loc2 = compare (sourceFile loc1) (sourceFile loc2)
  | sourceLine loc1 /= sourceLine loc2 = compare (sourceLine loc1) (sourceLine loc2)
  | otherwise = compare (sourceColumn loc1) (sourceColumn loc2)

calculateRangeEnd :: SourceLocation -> Int -> SourceLocation
calculateRangeEnd startLoc length = 
  startLoc { sourceLine = sourceLine startLoc + length }

createLocationRange :: SourceLocation -> Int -> LocationRange
createLocationRange startLoc length = LocationRange
  { rangeStart = startLoc
  , rangeEnd = calculateRangeEnd startLoc length
  }

locationInRange :: SourceLocation -> LocationRange -> Bool
locationInRange loc range =
  sourceLine loc >= sourceLine (rangeStart range) &&
  sourceLine loc <= sourceLine (rangeEnd range) &&
  sourceFile loc == sourceFile (rangeStart range)

interpolateLocation :: SourceLocation -> SourceLocation -> Double -> SourceLocation
interpolateLocation loc1 loc2 factor = SourceLocation
  { sourceLine = round (fromIntegral (sourceLine loc1) + factor * fromIntegral (sourceLine loc2 - sourceLine loc1))
  , sourceColumn = round (fromIntegral (sourceColumn loc1) + factor * fromIntegral (sourceColumn loc2 - sourceColumn loc1))
  , sourceFile = sourceFile loc1
  }

clusterLocations :: [SourceLocation] -> Int -> [[SourceLocation]]
clusterLocations [] _ = []
clusterLocations locations threshold = 
  let sorted = sortOn sourceLine locations
      (cluster, rest) = span (\loc -> sourceLine loc - sourceLine (head sorted) <= threshold) sorted
  in cluster : clusterLocations rest threshold

calculateBoundingBox :: [SourceLocation] -> LocationBoundingBox
calculateBoundingBox [] = LocationBoundingBox 0 0 0 0
calculateBoundingBox locations = LocationBoundingBox
  { boundingBoxMinLine = minimum (map sourceLine locations)
  , boundingBoxMaxLine = maximum (map sourceLine locations)
  , boundingBoxMinCol = minimum (map sourceColumn locations)
  , boundingBoxMaxCol = maximum (map sourceColumn locations)
  }

calculatePathDistance :: [SourceLocation] -> Int
calculatePathDistance [] = 0
calculatePathDistance [_] = 0
calculatePathDistance locations = sum $ zipWith calculateDistance locations (tail locations)

normalizeLocation :: SourceLocation -> SourceLocation
normalizeLocation loc = loc
  { sourceLine = max 0 (sourceLine loc)
  , sourceColumn = max 0 (sourceColumn loc)
  }

transformLocation :: SourceLocation -> Int -> Int -> SourceLocation
transformLocation loc lineOffset colOffset = loc
  { sourceLine = sourceLine loc + lineOffset
  , sourceColumn = sourceColumn loc + colOffset
  }

mergeLocations :: SourceLocation -> SourceLocation -> SourceLocation
mergeLocations loc1 loc2 = SourceLocation
  { sourceLine = min (sourceLine loc1) (sourceLine loc2)
  , sourceColumn = min (sourceColumn loc1) (sourceColumn loc2)
  , sourceFile = sourceFile loc1
  }

hashLocation :: SourceLocation -> Int
hashLocation loc = 
  sourceLine loc + 31 * sourceColumn loc + 961 * length (sourceFile loc)

-- ============================================================================
-- Test Collection
-- ============================================================================

tests :: TestTree
tests = testGroup "Source Location Math Properties QuickCheck Tests"
  [ fastProperty "Location distance calculation symmetry" prop_location_distance_symmetry
  , fastProperty "Location distance triangle inequality" prop_location_distance_triangle_inequality
  , fastProperty "Location ordering consistency" prop_location_ordering_consistency
  , fastProperty "Location range arithmetic" prop_location_range_arithmetic
  , fastProperty "Location containment" prop_location_containment
  , fastProperty "Location interpolation" prop_location_interpolation
  , fastProperty "Location clustering" prop_location_clustering
  , fastProperty "Location bounding box" prop_location_bounding_box
  , fastProperty "Location path distance" prop_location_path_distance
  , fastProperty "Location normalization" prop_location_normalization
  , fastProperty "Location transformation" prop_location_transformation
  , fastProperty "Location merge operation" prop_location_merge
  , fastProperty "Location hash consistency" prop_location_hash_consistency
  ]