#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.List (intercalate)

-- Import our new control flow ownership analyzer
import OwnershipControlFlow

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> testFile filename
        _ -> testAllExamples

testFile :: FilePath -> IO ()
testFile filename = do
    putStrLn $ "Testing file: " ++ filename
    errors <- analyzeOwnershipFile filename
    if null errors
        then putStrLn "✓ No ownership errors found!"
        else do
            putStrLn $ "✗ Found " ++ show (length errors) ++ " ownership errors:"
            mapM_ (putStrLn . ("  - " ++) . formatOwnershipError) errors
            exitFailure

testAllExamples :: IO ()
testAllExamples = do
    putStrLn "Testing control flow ownership analysis..."
    
    -- Test our new comprehensive test file
    putStrLn "\n1. Testing control flow ownership scenarios..."
    testFile "test_control_flow_ownership.typus"
    
    -- Test some simple cases
    putStrLn "\n2. Testing simple conditional ownership..."
    let simpleConditional = unlines
          [ "func testSimple() {"
          , "    let x = \"hello\""
          , "    let y = \"world\""
          , "    if x > y {"
          , "        let z = x  // move x in if branch"
          , "        println(z)"
          , "    } else {"
          , "        println(x)  // use x in else branch - should be safe"
          , "    }"
          , "    println(y)  // should be safe - y not moved"
          , "}"
          ]
    
    let errors1 = analyzeOwnership simpleConditional
    if null errors1
        then putStrLn "✓ Simple conditional test passed!"
        else do
            putStrLn $ "✗ Simple conditional test failed:"
            mapM_ (putStrLn . ("  - " ++) . formatOwnershipError) errors1
    
    -- Test problematic conditional
    putStrLn "\n3. Testing problematic conditional ownership..."
    let problematicConditional = unlines
          [ "func testProblematic() {"
          , "    let x = \"hello\""
          , "    let y = \"world\""
          , "    if x > y {"
          , "        let z = x  // move x in if branch"
          , "        println(z)"
          , "    } else {"
          , "        let w = y  // move y in else branch"
          , "        println(w)"
          , "    }"
          , "    println(x)  // should error - x may be moved"
          , "    println(y)  // should error - y may be moved"
          , "}"
          ]
    
    let errors2 = analyzeOwnership problematicConditional
    if not (null errors2)
        then do
            putStrLn "✓ Correctly detected ownership errors in problematic conditional!"
            putStrLn $ "  Found " ++ show (length errors2) ++ " errors:"
            mapM_ (putStrLn . ("  - " ++) . formatOwnershipError) errors2
        else do
            putStrLn "✗ Failed to detect ownership errors in problematic conditional!"
            exitFailure
    
    -- Test loop ownership
    putStrLn "\n4. Testing loop ownership patterns..."
    let loopTest = unlines
          [ "func testLoop() {"
          , "    let items = [\"a\", \"b\", \"c\"]"
          , "    let mut sum = \"\""
          , "    for item in items {"
          , "        let borrowed = &item  // borrow item"
          , "        println(borrowed)"
          , "        let temp = sum  // move sum"
          , "        sum = temp + item"
          , "    }"
          , "    println(sum)  // should be safe"
          , "}"
          ]
    
    let errors3 = analyzeOwnership loopTest
    if null errors3
        then putStrLn "✓ Loop ownership test passed!"
        else do
            putStrLn $ "✗ Loop ownership test failed:"
            mapM_ (putStrLn . ("  - " ++) . formatOwnershipError) errors3
    
    -- Test while loop
    putStrLn "\n5. Testing while loop ownership..."
    let whileTest = unlines
          [ "func testWhile() {"
          , "    let mut counter = 0"
          , "    let data = \"important\""
          , "    while counter < 3 {"
          , "        let borrowed = &data  // borrow data"
          , "        println(borrowed)"
          , "        counter = counter + 1"
          , "    }"
          , "    println(data)  // should be safe"
          , "}"
          ]
    
    let errors4 = analyzeOwnership whileTest
    if null errors4
        then putStrLn "✓ While loop ownership test passed!"
        else do
            putStrLn $ "✗ While loop ownership test failed:"
            mapM_ (putStrLn . ("  - " ++) . formatOwnershipError) errors4
    
    putStrLn "\n✓ All control flow ownership tests completed successfully!"

formatOwnershipError :: OwnershipError -> String
formatOwnershipError err = case err of
    UseAfterMove var -> "Use after move: " ++ var
    DoubleMove src dest -> "Double move: " ++ src ++ " to " ++ dest
    BorrowWhileMoved var -> "Borrow while moved: " ++ var
    MutBorrowWhileBorrowed var -> "Mutable borrow while borrowed: " ++ var
    BorrowWhileMutBorrowed var -> "Borrow while mut borrowed: " ++ var
    MultipleMutBorrows var -> "Multiple mutable borrows: " ++ var
    UseWhileMutBorrowed var -> "Use while mut borrowed: " ++ var
    OutOfScope var -> "Out of scope: " ++ var
    BorrowError var -> "Borrow error: " ++ var
    ParseError msg -> "Parse error: " ++ msg
    CrossFunctionMove src dest -> "Cross-function move: " ++ src ++ " to " ++ dest
    ParameterMoveMismatch param -> "Parameter move mismatch: " ++ param
    ControlFlowError msg -> "Control flow error: " ++ msg
    PathSensitiveError msg -> "Path sensitive error: " ++ msg
