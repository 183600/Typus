import Test.QuickCheck
import qualified Utils as U

-- 测试特定情况
testSpecificCase :: IO ()
testSpecificCase = do
    let testInput = "b"
    let withEscape = "\"" ++ testInput ++ "\\\""
    putStrLn $ "Input: " ++ show testInput
    putStrLn $ "With escape: " ++ show withEscape
    putStrLn $ "Length: " ++ show (length withEscape)
    putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral withEscape)
    putStrLn $ "isProblematicUnclosedString: " ++ show (U.isProblematicUnclosedString withEscape)
    
    -- 测试不同的情况
    putStrLn "\nTesting different cases:"
    let testCases = ["a\"", "b\"", "c\"", "a\\", "b\\", "c\\"]
    mapM_ (\caseStr -> do
        let quoted = "\"" ++ caseStr
        putStrLn $ quoted ++ " -> " ++ show (U.isProblematicUnclosedString quoted)
        ) testCases

main :: IO ()
main = testSpecificCase