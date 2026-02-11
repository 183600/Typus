import Data.Char (isSpace)
import Data.List (isInfixOf, elem)

main :: IO ()
main = do
    let s = "\v// comment"
    
    putStrLn $ "Input: " ++ show s
    putStrLn $ "null s: " ++ show (null s)
    putStrLn $ "s == \"\\n\": " ++ show (s == "\n")
    putStrLn $ "all isSpace s && s /= \"\\n\": " ++ show (all isSpace s && s /= "\n")
    putStrLn $ "s == \"//\": " ++ show (s == "//")
    putStrLn $ "s == \"'\": " ++ show (s == "'")
    putStrLn $ "s == \"/\": " ++ show (s == "/")
    putStrLn $ "length s == 1: " ++ show (length s == 1)
    putStrLn $ "// `isInfixOf` s: " ++ show ("//" `isInfixOf` s)
    putStrLn $ "not ('\\n' `elem` s): " ++ show (not ('\n' `elem` s))