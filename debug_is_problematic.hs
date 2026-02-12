import qualified Utils as U

main :: IO ()
main = do
    let input = "\"\\\\\""
    putStrLn $ "Input: " ++ show input
    putStrLn $ "isProblematicUnclosedString: " ++ show (U.isProblematicUnclosedString input)
    putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral input)
    putStrLn $ "not (isCompleteStringLiteral): " ++ show (not (U.isCompleteStringLiteral input))
    
    -- 检查所有相关的输入
    let inputs = ["\"", "\"\\\"", "\"\\\\\"", "\"\"", "\"\\\\"]
    mapM_ checkInput inputs
    
  where
    checkInput s = do
      putStrLn $ "\nChecking " ++ show s ++ ":"
      putStrLn $ "  isProblematicUnclosedString: " ++ show (U.isProblematicUnclosedString s)
      putStrLn $ "  isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral s)