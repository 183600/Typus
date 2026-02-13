import Data.Char

main :: IO ()
main = do
    let input = "\t  \n\n"
    let secondChar = case drop 1 input of (y:_) -> y; [] -> ' '
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Second char: " ++ show secondChar
    putStrLn $ "isSpace secondChar: " ++ show (isSpace secondChar)
    putStrLn $ "not (isSpace secondChar): " ++ show (not (isSpace secondChar))
    putStrLn $ "Combined condition 2: " ++ show (length input >= 2 && head input == '\t' && not (isSpace secondChar))