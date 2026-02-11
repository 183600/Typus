import Utils

main :: IO ()
main = do
    putStrLn $ "Utils.isCompleteStringLiteral \"\\\"\" = " ++ show (Utils.isCompleteStringLiteral "\"")
    putStrLn $ "Utils.isCompleteStringLiteral \"\\\"\\\"\" = " ++ show (Utils.isCompleteStringLiteral "\"\"")