import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    let input = "		 	"
    let result = normalizeIndentation input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    
    -- Check the conditions
    let inputLines = lines input
    putStrLn $ "Input lines: " ++ show inputLines
    putStrLn $ "Length of inputLines: " ++ show (length inputLines)
    
    case inputLines of
      [] -> putStrLn "Empty lines"
      [line] -> do
        putStrLn $ "Single line: " ++ show line
        putStrLn $ "Input == line: " ++ show (input == line)
        putStrLn $ "Input == "\t\t \t": " ++ show (input == "\t\t \t")
        putStrLn $ "All isSpace: " ++ show (all isSpace input)
        putStrLn $ "Input /= "\t": " ++ show (input /= "\t")
      _ -> putStrLn "Multiple lines"