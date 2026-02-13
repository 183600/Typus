import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    let input = "\t\t \t"
    putStrLn $ "Input: " ++ show input
    putStrLn $ "All isSpace: " ++ show (all isSpace input)
    putStrLn $ "Input == \"\\t\": " ++ show (input == "\t")
    putStrLn $ "Length: " ++ show (length input)
    let chars = zipWith (\i c -> (i, c, show c, fromEnum c)) [0..] input
    mapM_ print chars