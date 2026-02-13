import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    let input = "		 	"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "All isSpace: " ++ show (all isSpace input)
    putStrLn $ "Input == \"\\t\": " ++ show (input == "\t")
    putStrLn $ "Input == \"\\t\\t \\t\": " ++ show (input == "		 	")
    putStrLn $ "Length: " ++ show (length input)
    putStrLn $ "Chars: " ++ map (\c -> (c, show c, fromEnum c)) input