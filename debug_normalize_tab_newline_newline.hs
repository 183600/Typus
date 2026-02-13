import Utils

main :: IO ()
main = do
    let input = "\t  \n\n"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Input length: " ++ show (length input)
    putStrLn $ "Input chars: " ++ show (map (\c -> (c, fromEnum c)) input)
    let result = normalizeIndentation input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Result length: " ++ show (length result)
    putStrLn $ "Result chars: " ++ show (map (\c -> (c, fromEnum c)) result)