import Data.Char (chr, ord)
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

main :: IO ()
main = do
    let input = "\t\t \t"
    putStrLn $ "Input: " ++ show input
    
    -- Test endsWith
    let lastChar = safeLast input
    putStrLn $ "safeLast result: " ++ show lastChar ++ " (code: " ++ show (ord lastChar) ++ ")"
    
    let endsWithTab = endsWith input '\t'
    putStrLn $ "endsWith with '\\t': " ++ show endsWithTab
    
    -- Test isPrefixOf
    let hasPrefix = "\t\t" `isPrefixOf` input
    putStrLn $ "isPrefixOf \"\\t\\t\": " ++ show hasPrefix
    
    -- Test middle
    let middle = drop 2 (init input)
    putStrLn $ "middle: " ++ show middle
    
    -- Test isControlChar
    let isControlChar c = ord c < 32 || c == '\DEL'
    let hasControl = any isControlChar middle
    putStrLn $ "hasControl: " ++ show hasControl
    
    -- Test isSingleSpace
    let isSingleSpace = middle == " "
    putStrLn $ "isSingleSpace: " ++ show isSingleSpace