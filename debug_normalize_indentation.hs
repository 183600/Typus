import Utils

-- Test normalizeIndentation with tab
test1 :: IO ()
test1 = do
    let input = "\t"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: something other than \"\\t\""

-- Test normalizeIndentation with carriage return
test2 :: IO ()
test2 = do
    let input = "\r"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"    \" (4 spaces)"

-- Test normalizeIndentation with multiline mixed
test3 :: IO ()
test3 = do
    let input = "\nB"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: 1 line"

-- Test normalizeIndentation empty lines
test4 :: IO ()
test4 = do
    let input = ""
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"\""

-- Test removeLineComments multiline
test5 :: IO ()
test5 = do
    let input = "\n\a"
    let result = removeLineComments input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: 1 line"

main :: IO ()
main = do
    putStrLn "Test 1: normalizeIndentation with tab"
    test1
    putStrLn "\nTest 2: normalizeIndentation with carriage return"
    test2
    putStrLn "\nTest 3: normalizeIndentation with multiline mixed"
    test3
    putStrLn "\nTest 4: normalizeIndentation empty lines"
    test4
    putStrLn "\nTest 5: removeLineComments multiline"
    test5