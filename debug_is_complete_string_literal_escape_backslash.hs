import Utils

main :: IO ()
main = do
    let s = ""
    let withBackslash = "\"" ++ s ++ "\\\\"
    putStrLn $ "s = " ++ show s
    putStrLn $ "withBackslash = " ++ show withBackslash
    putStrLn $ "isCompleteStringLiteral withBackslash = " ++ show (Utils.isCompleteStringLiteral withBackslash)
    putStrLn $ "isCompleteStringLiteral \"\\\"\" = " ++ show (Utils.isCompleteStringLiteral "\"")