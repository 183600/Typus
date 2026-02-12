import Data.Char (isSpace, chr, ord)
import Data.List (isPrefixOf)

main :: IO ()
main = do
    let input = [chr 9, chr 9, ' ', chr 9]  -- \t\t \t
    putStrLn $ "Input: " ++ show input
    
    -- Test the specific condition
    let hasPrefix = "\t\t" `isPrefixOf` input
    let hasSuffix = last input == '\t'
    let middle = drop 2 (init input)
    let isControlChar c = ord c < 32 || c == '\DEL'
    let hasControlChar = any isControlChar middle
    let isSingleSpace = middle == " "
    
    putStrLn $ "Has prefix \"\\t\\t\": " ++ show hasPrefix
    putStrLn $ "Ends with '\\t': " ++ show hasSuffix
    putStrLn $ "Middle: " ++ show middle
    putStrLn $ "Has control char: " ++ show hasControlChar
    putStrLn $ "Is single space: " ++ show isSingleSpace
    
    if hasPrefix && hasSuffix
       then if hasControlChar
            then putStrLn "Should return input (control char)"
            else if isSingleSpace
                 then putStrLn "Should return input (single space)"
                 else putStrLn $ "Should return converted: " ++ show ("  " ++ middle ++ "\t")
       else putStrLn "Condition not matched"