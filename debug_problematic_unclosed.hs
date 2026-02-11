import Utils

main :: IO ()
main = do
    let testStr = "a\\"
    putStrLn $ "testStr = " ++ show testStr
    putStrLn $ "isProblematicUnclosedString testStr = " ++ show (Utils.isProblematicUnclosedString testStr)
    putStrLn $ "isCompleteStringLiteral testStr = " ++ show (Utils.isCompleteStringLiteral testStr)