import Utils
import Data.Char (isSpace)

main :: IO ()
main = do
    let line = " "
    putStrLn $ "line: " ++ show line
    putStrLn $ "line == \" \": " ++ show (line == " ")
    putStrLn $ "all isSpace line: " ++ show (all isSpace line)
    
    let result = if line == " "
                 then "    "
                 else if all isSpace line
                      then "    "
                      else line
    putStrLn $ "result: " ++ show result
    
    let actualResult = normalizeIndentation line
    putStrLn $ "normalizeIndentation result: " ++ show actualResult