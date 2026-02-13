import Data.Char

main :: IO ()
main = do
    let input = "\t  \n\n"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "all isSpace input: " ++ show (all isSpace input)
    putStrLn $ "length input >= 2: " ++ show (length input >= 2)
    putStrLn $ "head input == '\\t': " ++ show (head input == '\t')
    putStrLn $ "not (all isSpace input): " ++ show (not (all isSpace input))
    putStrLn $ "Combined condition: " ++ show (length input >= 2 && head input == '\t' && not (all isSpace input))