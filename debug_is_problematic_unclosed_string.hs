import Utils (isProblematicUnclosedString)

main :: IO ()
main = do
    putStrLn "Testing isProblematicUnclosedString:"
    putStrLn $ "isProblematicUnclosedString \">\" = " ++ show (isProblematicUnclosedString ">")
    putStrLn $ "isProblematicUnclosedString \"\\\"\" = " ++ show (isProblematicUnclosedString "\"")
    putStrLn $ "isProblematicUnclosedString \"'\" = " ++ show (isProblematicUnclosedString "'")
    putStrLn $ "isProblematicUnclosedString \"\\\"\\\"\" = " ++ show (isProblematicUnclosedString "\"\"")
    putStrLn $ "isProblematicUnclosedString \"\" = " ++ show (isProblematicUnclosedString "")