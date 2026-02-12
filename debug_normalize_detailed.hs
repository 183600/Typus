import Data.Char (chr, ord, isSpace, isPrint)
import Data.List (isPrefixOf)

-- Replicate the functions
safeLast :: String -> Char
safeLast [] = '\0'  -- 默认值，调用者需要检查
safeLast xs = case reverse xs of
                [] -> '\0'
                (c:_) -> c

endsWith :: String -> Char -> Bool
endsWith [] _ = False
endsWith s c = safeLast s == c

safeInit :: String -> String
safeInit [] = []
safeInit xs = init xs

-- Replicate the normalizeIndentation logic
normalizeIndentationDebug :: String -> IO String
normalizeIndentationDebug input = do
    putStrLn $ "=== normalizeIndentation called with: " ++ show input
    
    -- Check the specific condition
    if "\t\t" `isPrefixOf` input && endsWith input '\t'
       then do
           putStrLn $ "Condition 1 matched: \"\\t\\t\" prefix and \"\\t\" suffix"
           let middle = drop 2 (init input)
           putStrLn $ "middle: " ++ show middle
           let isControlChar c = ord c < 32 || c == '\DEL'
           let hasControl = any isControlChar middle
           putStrLn $ "hasControl: " ++ show hasControl
           let isSingleSpace = middle == " "
           putStrLn $ "isSingleSpace: " ++ show isSingleSpace
           
           if hasControl
              then do
                  putStrLn "Returning input (control char)"
                  return input
              else if isSingleSpace
                   then do
                       putStrLn "Returning input (single space)"
                       return input
                   else do
                       let result = "  " ++ middle ++ "\t"
                       putStrLn $ "Returning converted: " ++ show result
                       return result
       else do
           putStrLn $ "Condition 1 NOT matched"
           return "NOT MATCHED"

main :: IO ()
main = do
    let input = "\t\t \t"
    result <- normalizeIndentationDebug input
    putStrLn $ "=== Final result: " ++ show result