import Data.Char (isSpace)

main :: IO ()
main = do
    let line = "\t  "
    putStrLn $ "line: " ++ show line
    putStrLn $ "all isSpace line: " ++ show (all isSpace line)
    putStrLn $ "line == \" \": " ++ show (line == " ")
    
    let result = if line == " "
                 then "    "
                 else if all isSpace line
                      then "    "
                      else line
    putStrLn $ "result: " ++ show result