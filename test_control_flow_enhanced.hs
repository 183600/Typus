#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Data.List (intercalate)

-- Import our enhanced control flow ownership analyzer
import OwnershipControlFlowEnhanced

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
    putStrLn "Testing enhanced control flow ownership analysis..."
    
    -- Test 1: 条件分支中的所有权转移
    putStrLn "\n1. Testing conditional ownership transfer..."
    let conditionalTest = unlines
          [ "func testConditionalTransfer() {"
          , "    let x = \"hello\""
          , "    let y = \"world\""
          , "    "
          , "    if x > y {"
          , "        // 在if分支中移动x"
          , "        let z = x"
          , "        println(z)"
          , "    } else {"
          , "        // 在else分支中移动y"
          , "        let w = y"
          , "        println(w)"
          , "    }"
          , "    "
          , "    // 这里应该检测到错误：x和y都可能在某个分支被移动"
          , "    println(x)  // ✗ 应该报错 - x可能在if分支被移动"
          , "    println(y)  // ✗ 应该报错 - y可能在else分支被移动"
          , "}"
          ]
    testCode "Conditional Transfer Test" conditionalTest
    
    -- Test 2: 循环中的所有权模式
    putStrLn "\n2. Testing loop ownership patterns..."
    let loopTest = unlines
          [ "func testLoopPatterns() {"
          , "    let items = [\"a\", \"b\", \"c\"]"
          , "    let mut sum = \"\""
          , "    "
          , "    for item in items {"
          , "        // 循环中借用item"
          , "        let borrowed = &item"
          , "        println(borrowed)"
          , "        "
          , "        // 在循环中移动sum - 每次迭代都会重新创建"
          , "        let temp = sum"
          , "        sum = temp + item"
          , "    }"
          , "    "
          , "    // 循环后使用sum应该是安全的"
          , "    println(sum)  // ✓ 应该安全"
          , "}"
          ]
    testCode "Loop Patterns Test" loopTest
    
    -- Test 3: 嵌套条件分支
    putStrLn "\n3. Testing nested conditional branches..."
    let nestedTest = unlines
          [ "func testNestedBranches() {"
          , "    let a = \"first\""
          , "    let b = \"second\""
          , "    let c = \"third\""
          , "    "
          , "    if a > b {"
          , "        if b > c {"
          , "            // 深层嵌套中的所有权转移"
          , "            let temp1 = a"
          , "            let temp2 = b"
          , "            println(temp1)"
          , "            println(temp2)"
          , "        } else {"
          , "            let temp3 = c"
          , "            println(temp3)"
          , "        }"
          , "        // 这里a和b可能已经被移动"
          , "        println(a)  // ✗ 应该报错 - a可能在深层嵌套被移动"
          , "        println(b)  // ✗ 应该报错 - b可能在深层嵌套被移动"
          , "    } else {"
          , "        // 这里a没有被移动，b也没有被移动"
          , "        println(a)  // ✓ 应该安全"
          , "        println(b)  // ✓ 应该安全"
          , "    }"
          , "    "
          , "    // 路径敏感分析应该能确定c始终可用"
          , "    println(c)  // ✓ 应该安全"
          , "}"
          ]
    testCode "Nested Branches Test" nestedTest
    
    -- Test 4: while循环中的所有权
    putStrLn "\n4. Testing while loop ownership..."
    let whileTest = unlines
          [ "func testWhileLoopOwnership() {"
          , "    let mut counter = 0"
          , "    let data = \"important\""
          , "    "
          , "    while counter < 3 {"
          , "        // 在循环中借用data"
          , "        let borrowed_data = &data"
          , "        println(borrowed_data)"
          , "        "
          , "        // 修改计数器"
          , "        counter = counter + 1"
          , "    }"
          , "    "
          , "    // 循环后data仍然可用"
          , "    println(data)  // ✓ 应该安全"
          , "}"
          ]
    testCode "While Loop Test" whileTest
    
    -- Test 5: 分支中的借用模式
    putStrLn "\n5. Testing borrow patterns in branches..."
    let borrowTest = unlines
          [ "func testBorrowInBranches() {"
          , "    let data = \"shared_data\""
          , "    let condition = true"
          , "    "
          , "    if condition {"
          , "        // 在if分支中可变借用"
          , "        let mut_ref = &mut data"
          , "        // 使用mut_ref..."
          , "    } else {"
          , "        // 在else分支中不可变借用"
          , "        let ref = &data"
          , "        // 使用ref..."
          , "    }"
          , "    "
          , "    // 两个分支都结束后，data应该可用"
          , "    println(data)  // ✓ 应该安全"
          , "}"
          ]
    testCode "Borrow in Branches Test" borrowTest
    
    -- Test 6: 复杂控制流组合
    putStrLn "\n6. Testing complex control flow combinations..."
    let complexTest = unlines
          [ "func testComplexControlFlow() {"
          , "    let resource1 = \"resource1\""
          , "    let resource2 = \"resource2\""
          , "    let mut flag = true"
          , "    "
          , "    if flag {"
          , "        for i in [1, 2, 3] {"
          , "            if i > 1 {"
          , "                // 条件移动"
          , "                let temp = resource1"
          , "                println(temp)"
          , "                flag = false"
          , "            }"
          , "        }"
          , "    } else {"
          , "        // 另一个分支"
          , "        let temp2 = resource2"
          , "        println(temp2)"
          , "    }"
          , "    "
          , "    // 路径敏感分析应该能跟踪resource1和resource2的状态"
          , "    // 基于flag的值和循环的执行情况"
          , "    println(resource1)  // ✗ 可能在循环中被移动"
          , "    println(resource2)  // ✗ 可能在else分支中被移动"
          , "}"
          ]
    testCode "Complex Control Flow Test" complexTest
    
    -- Test 7: 循环中的借用模式
    putStrLn "\n7. Testing borrow patterns in loops..."
    let loopBorrowTest = unlines
          [ "func testLoopBorrowPatterns() {"
          , "    let mut collection = [\"item1\", \"item2\", \"item3\"]"
          , "    "
          , "    for item in collection {"
          , "        // 在循环中借用item"
          , "        let item_ref = &item"
          , "        println(item_ref)"
          , "        "
          , "        // 通过可变引用修改集合"
          , "        let mut_ref = &mut collection"
          , "        // 注意：这可能导致迭代器失效，应该被检测到"
          , "    }"
          , "}"
          ]
    testCode "Loop Borrow Patterns Test" loopBorrowTest
    
    putStrLn "\n✓ All control flow ownership tests completed!"

testCode :: String -> String -> IO ()
testCode testName code = do
    putStrLn $ "\n--- " ++ testName ++ " ---"
    errors <- return $ analyzeOwnership code
    if null errors
        then putStrLn "✓ No ownership errors found!"
        else do
            putStrLn $ "✗ Found " ++ show (length errors) ++ " ownership errors:"
            mapM_ (putStrLn . ("  - " ++) . formatOwnershipError) errors

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
    LoopOwnershipError msg -> "Loop ownership error: " ++ msg
