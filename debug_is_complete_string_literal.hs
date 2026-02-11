import Utils

main :: IO ()
main = do
    let s = "\\"
    putStrLn $ "s = " ++ show s
    putStrLn $ "isCompleteStringLiteral s = " ++ show (Utils.isCompleteStringLiteral s)