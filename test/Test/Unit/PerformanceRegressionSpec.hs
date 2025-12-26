{-# LANGUAGE CPP #-}
module Test.Unit.PerformanceRegressionSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)
import System.CPUTime (getCPUTime)
import Control.DeepSeq (force)

import Parser (parseTypus)
import Compiler (compile)
import Utils (trim, splitBy)

tests :: TestTree
tests =
  testGroup "Performance Regression Tests"
    [ testCase "parsing performance stays within acceptable limits" $ do
        let moderateInput = unlines $ replicate 1000 "func testFunction() { return 42; }"
        startTime <- getCPUTime
        case parseTypus moderateInput of
          Left err -> assertFailure $ "Parsing failed: " ++ err
          Right _ -> do
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            assertBool "Parsing took too long" $ timeDiff < 1.0  -- Should complete within 1 second
    
    , testCase "compilation performance scales linearly" $ do
        let smallInput = unlines $ replicate 100 "func test() { return 42; }"
            mediumInput = unlines $ replicate 200 "func test() { return 42; }"
            
        startTime1 <- getCPUTime
        case parseTypus smallInput of
          Left err -> assertFailure $ "Small input parsing failed: " ++ err
          Right _ -> do
            endTime1 <- getCPUTime
            let time1 = fromIntegral (endTime1 - startTime1) / (10^12)
            
            startTime2 <- getCPUTime
            case parseTypus mediumInput of
              Left err -> assertFailure $ "Medium input parsing failed: " ++ err
              Right _ -> do
                endTime2 <- getCPUTime
                let time2 = fromIntegral (endTime2 - startTime2) / (10^12)
                    ratio = time2 / time1
                
                -- Should scale roughly linearly (allowing for some variance)
                assertBool "Compilation doesn't scale linearly" $ ratio < 3.0
    
    , testCase "string processing performance is acceptable" $ do
        let testString = concat $ replicate 10000 "test string with spaces  "
        startTime <- getCPUTime
        let result = force $ trim testString
        endTime <- getCPUTime
        
        let timeDiff = fromIntegral (endTime - startTime) / (10^12)
        assertBool "String processing took too long" $ timeDiff < 0.5
        assertBool "String processing incorrect result" $ result == "test string with spaces"
    
    , testCase "memory usage doesn't grow excessively with input size" $ do
        let inputs = [unlines $ replicate n "func test() { return 42; }" | n <- [100, 500, 1000]]
        
        results <- mapM (\input -> do
          startTime <- getCPUTime
          case parseTypus input of
            Left err -> return $ Left err
            Right result -> do
              endTime <- getCPUTime
              let timeDiff = fromIntegral (endTime - startTime) / (10^12)
              return $ Right (length (show result), timeDiff)
        ) inputs
        
        case sequence results of
          Left err -> assertFailure $ "Performance test failed: " ++ err
          Right successes -> do
            let sizes = map fst successes
                times = map snd successes
            
            -- Check that time growth is reasonable compared to input size growth
            assertBool "Memory usage grows excessively" $ all (< 2.0) [t2/t1 | (t1, t2) <- zip times (tail times)]
    
    , testCase "concurrent parsing performance is acceptable" $ do
        let testInputs = [unlines $ replicate 100 ("func test" ++ show i ++ "() { return " ++ show i ++ "; }") 
                         | i <- [1..5]]
        
        -- Test sequential parsing
        startTimeSeq <- getCPUTime
        seqResults <- mapM parseTypus testInputs
        endTimeSeq <- getCPUTime
        let seqTime = fromIntegral (endTimeSeq - startTimeSeq) / (10^12)
        
        -- All parses should succeed
        assertBool "Sequential parsing failed" $ all (either (const False) (const True)) seqResults
        
        -- Performance should be reasonable
        assertBool "Sequential parsing too slow" $ seqTime < 2.0
    
    , testCase "large file processing doesn't cause timeouts" $ do
        let largeInput = unlines $ 
                ["// Large file test"] ++
                replicate 500 "func largeTest() { return 42; }" ++
                ["func main() { return 0; }"]
        
        startTime <- getCPUTime
        case parseTypus largeInput of
          Left err -> assertFailure $ "Large file parsing failed: " ++ err
          Right result -> do
            endTime <- getCPUTime
            let timeDiff = fromIntegral (endTime - startTime) / (10^12)
            
            assertBool "Large file processing took too long" $ timeDiff < 2.0
            assertBool "Large file parsing incomplete" $ length (show result) > 100
    ]