import Utils

main :: IO ()
main = do
    putStrLn $ "splitBy '\\n' \"\\na\" = " ++ show (splitBy '\n' "\na")
    putStrLn $ "Expected: [\"\", \"a\"]"