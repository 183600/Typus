import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
    -- 检查 removeLineComments 如何处理 "\"\"p// not comment\""
    let withSlash = "\"\"p// not comment\""
    let processed = Utils.removeLineComments withSlash
    putStrLn $ "withSlash = " ++ show withSlash
    putStrLn $ "processed = " ++ show processed
    
    -- 逐步分析
    putStrLn "\nStep by step analysis:"
    putStrLn $ "First char: " ++ show (head withSlash)
    putStrLn $ "Rest: " ++ show (tail withSlash)
    
    -- 检查 isProblematicUnclosedString 对 "\"p// not comment\"" 的行为
    let testStr = "\"p// not comment\""
    putStrLn $ "\nisProblematicUnclosedString testStr = " ++ show (Utils.isProblematicUnclosedString testStr)
    
    -- 检查 removeLineComments 对 "\"p// not comment\"" 的行为
    let testProcessed = Utils.removeLineComments testStr
    putStrLn $ "removeLineComments testStr = " ++ show testProcessed