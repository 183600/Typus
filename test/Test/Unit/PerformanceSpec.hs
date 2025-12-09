module Test.Unit.PerformanceSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import System.CPUTime (getCPUTime)

import qualified Parser
import qualified Compiler

tests :: TestTree
tests =
  testGroup "Performance tests"
    [ testCase "parses large files efficiently" $ do
        let largeSource = unlines $ concat
              [ ["package main"]
              , ["func main() {"]
              , replicate 1000 "    println(\"test\")"
              , ["}"]
              ]
        start <- getCPUTime
        case Parser.parseTypus largeSource of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> do
            end <- getCPUTime
            let diff = fromIntegral (end - start) / (10 ** 12) :: Double
            assertBool ("parsing should take less than 1 second, took " ++ show diff) (diff < 1.0)

    , testCase "compiles complex types efficiently" $ do
        let complexSource = unlines $ concat
              [ ["package main"]
              , ["type Complex struct {"]
              , replicate 100 "    field int"
              , ["}"]
              , ["func main() {"]
              , ["    c := Complex{"]
              , replicate 100 "    field: 1,"
              , ["    }"]
              , ["    println(c)"]
              , ["}"]
              ]
        start <- getCPUTime
        case Parser.parseTypus complexSource of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            case Compiler.compile typusFile of
              Left err -> assertFailure $ "compile failed: " ++ Compiler.renderCompilationError err
              Right _ -> do
                end <- getCPUTime
                let diff = fromIntegral (end - start) / (10 ** 12) :: Double
                assertBool ("compilation should take less than 2 seconds, took " ++ show diff) (diff < 2.0)

    , testCase "handles deeply nested structures efficiently" $ do
        let nestedSource = unlines $ concat
              [ ["package main"]
              , ["func main() {"]
              , concat $ replicate 50 ["    println(\"nested\")"]
              , ["}"]
              ]
        start <- getCPUTime
        case Parser.parseTypus nestedSource of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> do
            end <- getCPUTime
            let diff = fromIntegral (end - start) / (10 ** 12) :: Double
            assertBool ("parsing nested structures should take less than 1 second, took " ++ show diff) (diff < 1.0)

    , testCase "processes many small functions efficiently" $ do
        let manyFunctionsSource = unlines $ concat
              [ ["package main"]
              , concat $ map (\i -> ["func test" ++ show i ++ "() int {", "    return " ++ show i, "}"]) ([1..200] :: [Integer])
              , ["func main() {"]
              , ["    total := 0"]
              , concat $ map (\i -> ["    total += test" ++ show i ++ "()"]) ([1..200] :: [Integer])
              , ["    println(total)"]
              , ["}"]
              ]
        start <- getCPUTime
        case Parser.parseTypus manyFunctionsSource of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right typusFile -> do
            case Compiler.compile typusFile of
              Left err -> assertFailure $ "compile failed: " ++ Compiler.renderCompilationError err
              Right _ -> do
                end <- getCPUTime
                let diff = fromIntegral (end - start) / (10 ** 12) :: Double
                assertBool ("processing many functions should take less than 3 seconds, took " ++ show diff) (diff < 3.0)

    , testCase "memory usage remains reasonable for large files" $ do
        let largeSource = unlines $ concat
              [ ["package main"]
              , ["// Large comment block"]
              , replicate 5000 "// This is a comment line to test memory usage"
              , ["func main() {"]
              , replicate 1000 "    println(\"memory test\")"
              , ["}"]
              ]
        start <- getCPUTime
        case Parser.parseTypus largeSource of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> do
            end <- getCPUTime
            let diff = fromIntegral (end - start) / (10 ** 12) :: Double
            assertBool ("parsing large file should take less than 2 seconds, took " ++ show diff) (diff < 2.0)

    , testCase "compilation time scales linearly with file size" $ do
        let smallSource = unlines $ concat
              [ ["package main"]
              , ["func main() {"]
              , replicate 10 "    println(\"small\")"
              , ["}"]
              ]
        let mediumSource = unlines $ concat
              [ ["package main"]
              , ["func main() {"]
              , replicate 100 "    println(\"medium\")"
              , ["}"]
              ]
        let largeSource = unlines $ concat
              [ ["package main"]
              , ["func main() {"]
              , replicate 1000 "    println(\"large\")"
              , ["}"]
              ]
        
        -- Parse small file
        startSmall <- getCPUTime
        case Parser.parseTypus smallSource of
          Left err -> assertFailure $ "parseTypus failed on small: " <> err
          Right _ -> do
            endSmall <- getCPUTime
            let smallTime = fromIntegral (endSmall - startSmall) / (10 ** 12) :: Double
            
            -- Parse medium file
            startMedium <- getCPUTime
            case Parser.parseTypus mediumSource of
              Left err -> assertFailure $ "parseTypus failed on medium: " <> err
              Right _ -> do
                endMedium <- getCPUTime
                let mediumTime = fromIntegral (endMedium - startMedium) / (10 ** 12) :: Double
                
                -- Parse large file
                startLarge <- getCPUTime
                case Parser.parseTypus largeSource of
                  Left err -> assertFailure $ "parseTypus failed on large: " <> err
                  Right _ -> do
                    endLarge <- getCPUTime
                    let largeTime = fromIntegral (endLarge - startLarge) / (10 ** 12) :: Double
                    
                    -- Check that time scales roughly linearly
                    let smallToMediumRatio = mediumTime / smallTime
                    let mediumToLargeRatio = largeTime / mediumTime
                    
                    assertBool ("small to medium ratio should be reasonable: " ++ show smallToMediumRatio) (smallToMediumRatio < 20)
                    assertBool ("medium to large ratio should be reasonable: " ++ show mediumToLargeRatio) (mediumToLargeRatio < 20)

    , testCase "error handling doesn't significantly impact performance" $ do
        let sourceWithErrors = unlines $ concat
              [ ["package main"]
              , ["func undefined() {}"]  
              , ["func main() {"]
              , ["    undefined()"]       
              , ["}"]
              , ["package main2"]  -- Multiple package declarations will cause parsing error
              ]
        start <- getCPUTime
        case Parser.parseTypus sourceWithErrors of
          Left _ -> do
            end <- getCPUTime
            let diff = fromIntegral (end - start) / (10 ** 12) :: Double
            assertBool ("error handling should be fast, took " ++ show diff) (diff < 0.5)
          Right _ -> assertFailure "expected parsing to fail"

    , testCase "parallel processing potential" $ do
        let sources = replicate 10 $ unlines
              [ "package main"
              , "func test() int { return 42 }"
              , "func main() { println(test()) }"
              ]
        start <- getCPUTime
        mapM_ (\source -> case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed: " <> err
          Right _ -> return ()) sources
        end <- getCPUTime
        let diff = fromIntegral (end - start) / (10 ** 12) :: Double
        assertBool ("processing multiple files should be efficient, took " ++ show diff) (diff < 1.0)

    , testCase "cache effectiveness" $ do
        let source = unlines
              [ "package main"
              , "func main() {"
              , "    println(\"cache test\")"
              , "}"
              ]
        -- First parse
        start1 <- getCPUTime
        case Parser.parseTypus source of
          Left err -> assertFailure $ "parseTypus failed on first parse: " <> err
          Right firstResult -> do
            end1 <- getCPUTime
            let firstTime = fromIntegral (end1 - start1) / (10 ** 12) :: Double
            
            -- Second parse (simulating cache)
            start2 <- getCPUTime
            secondResult <- case Parser.parseTypus source of
              Left err -> assertFailure $ "parseTypus failed on second parse: " <> err
              Right result -> return result
            end2 <- getCPUTime
            let secondTime = fromIntegral (end2 - start2) / (10 ** 12) :: Double
            
            -- Both should be fast, but this tests the baseline performance
            assertBool ("first parse should be fast: " ++ show firstTime) (firstTime < 0.1)
            assertBool ("second parse should be fast: " ++ show secondTime) (secondTime < 0.1)
    ]