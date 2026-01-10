module Test.Unit.SourceLocationMathSpec where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import qualified SourceLocation as SL
import qualified SourceLocation.Math as SLM
import Data.Maybe (isJust, isNothing)

-- 测试源码位置数学运算的属性
prop_location_addition :: Int -> Int -> Int -> Int -> Property
prop_location_addition line1 col1 line2 col2 = 
  let loc1 = SL.SourceLocation line1 col1
      loc2 = SL.SourceLocation line2 col2
      result = SLM.addLocations loc1 loc2
  in property $ SL.line result === line1 + line2 && 
             SL.column result === col1 + col2

prop_location_subtraction :: Int -> Int -> Int -> Int -> Property
prop_location_subtraction line1 col1 line2 col2 = 
  let loc1 = SL.SourceLocation line1 col1
      loc2 = SL.SourceLocation line2 col2
      result = SLM.subtractLocations loc1 loc2
  in property $ SL.line result === line1 - line2 && 
             SL.column result === col1 - col2

prop_location_distance :: Int -> Int -> Int -> Int -> Property
prop_location_distance line1 col1 line2 col2 = 
  let loc1 = SL.SourceLocation line1 col1
      loc2 = SL.SourceLocation line2 col2
      distance = SLM.distance loc1 loc2
  in property $ distance >= 0

prop_location_midpoint :: Int -> Int -> Int -> Int -> Property
prop_location_midpoint line1 col1 line2 col2 = 
  let loc1 = SL.SourceLocation line1 col1
      loc2 = SL.SourceLocation line2 col2
      midpoint = SLM.midpoint loc1 loc2
  in property $ SL.line midpoint === (line1 + line2) `div` 2 && 
             SL.column midpoint === (col1 + col2) `div` 2

prop_location_manhattan_distance :: Int -> Int -> Int -> Int -> Property
prop_location_manhattan_distance line1 col1 line2 col2 = 
  let loc1 = SL.SourceLocation line1 col1
      loc2 = SL.SourceLocation line2 col2
      distance = SLM.manhattanDistance loc1 loc2
  in property $ distance === abs (line1 - line2) + abs (col1 - col2)

prop_location_euclidean_distance :: Int -> Int -> Int -> Int -> Property
prop_location_euclidean_distance line1 col1 line2 col2 = 
  let loc1 = SL.SourceLocation line1 col1
      loc2 = SL.SourceLocation line2 col2
      distance = SLM.euclideanDistance loc1 loc2
  in property $ distance >= 0

prop_location_scaling :: Int -> Int -> Int -> Property
prop_location_scaling line col factor = 
  let loc = SL.SourceLocation line col
      scaled = SLM.scaleLocation loc factor
  in property $ SL.line scaled === line * factor && 
             SL.column scaled === col * factor

prop_location_translation :: Int -> Int -> Int -> Int -> Property
prop_location_translation line col dline dcol = 
  let loc = SL.SourceLocation line col
      translated = SLM.translateLocation loc dline dcol
  in property $ SL.line translated === line + dline && 
             SL.column translated === col + dcol

prop_location_rotation :: Int -> Int -> Int -> Property
prop_location_rotation line col angle = 
  let loc = SL.SourceLocation line col
      rotated = SLM.rotateLocation loc angle
  in property $ SL.line rotated >= 0 && SL.column rotated >= 0

prop_location_reflection :: Int -> Int -> Property
prop_location_reflection line col = 
  let loc = SL.SourceLocation line col
      reflected = SLM.reflectLocation loc
  in property $ SL.line reflected === line && 
             SL.column reflected === -col

prop_location_bounds :: [Int] -> [Int] -> Property
prop_location_bounds lines cols = 
  let locations = zipWith SL.SourceLocation lines cols
      bounds = SLM.calculateBounds locations
  in property $ isJust bounds

prop_location_enclosure :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_location_enclosure line1 col1 line2 col2 line3 col3 line4 = 
  let loc1 = SL.SourceLocation line1 col1
      loc2 = SL.SourceLocation line2 col2
      loc3 = SL.SourceLocation line3 col3
      loc4 = SL.SourceLocation line4 col4
      rectangle = SLM.createRectangle loc1 loc2
      enclosed = SLM.isEnclosed loc3 rectangle
  in property $ enclosed === (line3 >= min line1 line2 && line3 <= max line1 line2 &&
                             col3 >= min col1 col2 && col3 <= max col1 col2)

prop_location_intersection :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_location_intersection line1 col1 line2 col2 line3 col3 line4 col4 = 
  let rect1 = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      rect2 = SLM.createRectangle (SL.SourceLocation line3 col3) (SL.SourceLocation line4 col4)
      intersection = SLM.intersection rect1 rect2
  in property $ isJust intersection

prop_location_union :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_location_union line1 col1 line2 col2 line3 col3 line4 col4 = 
  let rect1 = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      rect2 = SLM.createRectangle (SL.SourceLocation line3 col3) (SL.SourceLocation line4 col4)
      union = SLM.union rect1 rect2
  in property $ isJust union

prop_location_area :: Int -> Int -> Int -> Int -> Property
prop_location_area line1 col1 line2 col2 = 
  let rect = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      area = SLM.area rect
  in property $ area >= 0

prop_location_perimeter :: Int -> Int -> Int -> Int -> Property
prop_location_perimeter line1 col1 line2 col2 = 
  let rect = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      perimeter = SLM.perimeter rect
  in property $ perimeter >= 0

prop_location_center :: Int -> Int -> Int -> Int -> Property
prop_location_center line1 col1 line2 col2 = 
  let rect = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      center = SLM.center rect
  in property $ SL.line center === (line1 + line2) `div` 2 && 
             SL.column center === (col1 + col2) `div` 2

prop_location_expansion :: Int -> Int -> Int -> Int -> Int -> Property
prop_location_expansion line1 col1 line2 col2 amount = 
  let rect = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      expanded = SLM.expand rect amount
  in property $ SLM.area expanded >= SLM.area rect

prop_location_contraction :: Int -> Int -> Int -> Int -> Int -> Property
prop_location_contraction line1 col1 line2 col2 amount = 
  let rect = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      contracted = SLM.contract rect amount
  in property $ SLM.area contracted <= SLM.area rect

prop_location_contains :: Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_location_contains line1 col1 line2 col2 line3 col3 = 
  let rect = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      point = SL.SourceLocation line3 col3
      contains = SLM.contains rect point
  in property $ contains === (line3 >= min line1 line2 && line3 <= max line1 line2 &&
                              col3 >= min col1 col2 && col3 <= max col1 col2)

prop_location_overlaps :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_location_overlaps line1 col1 line2 col2 line3 col3 line4 col4 = 
  let rect1 = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      rect2 = SLM.createRectangle (SL.SourceLocation line3 col3) (SL.SourceLocation line4 col4)
      overlaps = SLM.overlaps rect1 rect2
  in property $ overlaps === (not (null (SLM.intersection rect1 rect2)))

prop_location_adjacent :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Property
prop_location_adjacent line1 col1 line2 col2 line3 col3 line4 col4 = 
  let rect1 = SLM.createRectangle (SL.SourceLocation line1 col1) (SL.SourceLocation line2 col2)
      rect2 = SLM.createRectangle (SL.SourceLocation line3 col3) (SL.SourceLocation line4 col4)
      adjacent = SLM.isAdjacent rect1 rect2
  in property $ adjacent === (not (SLM.overlaps rect1 rect2) && 
                              SLM.distance (SLM.center rect1) (SLM.center rect2) <= 2)

prop_location_path :: [Int] -> [Int] -> Property
prop_location_path lines cols = 
  let locations = zipWith SL.SourceLocation lines cols
      path = SLM.createPath locations
  in property $ length path >= 0

prop_location_path_length :: [Int] -> [Int] -> Property
prop_location_path_length lines cols = 
  let locations = zipWith SL.SourceLocation lines cols
      path = SLM.createPath locations
      length = SLM.pathLength path
  in property $ length >= 0

prop_location_path_optimization :: [Int] -> [Int] -> Property
prop_location_path_optimization lines cols = 
  let locations = zipWith SL.SourceLocation lines cols
      path = SLM.createPath locations
      optimized = SLM.optimizePath path
  in property $ SLM.pathLength optimized <= SLM.pathLength path

tests :: TestTree
tests = testGroup "Source Location Math Tests"
  [ testProperty "Location addition" prop_location_addition
  , testProperty "Location subtraction" prop_location_subtraction
  , testProperty "Location distance" prop_location_distance
  , testProperty "Location midpoint" prop_location_midpoint
  , testProperty "Location manhattan distance" prop_location_manhattan_distance
  , testProperty "Location euclidean distance" prop_location_euclidean_distance
  , testProperty "Location scaling" prop_location_scaling
  , testProperty "Location translation" prop_location_translation
  , testProperty "Location rotation" prop_location_rotation
  , testProperty "Location reflection" prop_location_reflection
  , testProperty "Location bounds" prop_location_bounds
  , testProperty "Location enclosure" prop_location_enclosure
  , testProperty "Location intersection" prop_location_intersection
  , testProperty "Location union" prop_location_union
  , testProperty "Location area" prop_location_area
  , testProperty "Location perimeter" prop_location_perimeter
  , testProperty "Location center" prop_location_center
  , testProperty "Location expansion" prop_location_expansion
  , testProperty "Location contraction" prop_location_contraction
  , testProperty "Location contains" prop_location_contains
  , testProperty "Location overlaps" prop_location_overlaps
  , testProperty "Location adjacent" prop_location_adjacent
  , testProperty "Location path" prop_location_path
  , testProperty "Location path length" prop_location_path_length
  , testProperty "Location path optimization" prop_location_path_optimization
  ]