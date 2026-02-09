import Utils

main :: IO ()
main = do
    putStrLn $ "isCompleteStringLiteral \"\\\\\"\\\\\"\": " ++ show (isCompleteStringLiteral "\"\\\\\"")
    putStrLn $ "isProblematicUnclosedString \"\\\\\"\\\\\"\": " ++ show (isProblematicUnclosedString "\"\\\\\"")