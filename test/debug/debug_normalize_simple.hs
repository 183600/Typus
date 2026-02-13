import Utils (normalizeIndentation)
import Data.Char (ord, isSpace)
import Data.List (isPrefixOf, isInfixOf)

main :: IO ()
main = do
    let input = "\t  \t  \rP  \t  "
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Input length: " ++ show (length input)
    
    -- Check each character
    putStrLn $ "Characters:"
    mapM_ (\c -> putStrLn $ "  " ++ show c ++ " (ord: " ++ show (ord c) ++ ")") input
    
    -- Check conditions
    let controlChars = ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL', '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK']
    let hasControlChar = any (`elem` controlChars) input
    let notSpecialCase = input /= "\t  \t  \r  \t  " && input /= "\f" && input /= "\r"
    let noBraces = not (any (`isInfixOf` input) ["{", "}"])
    
    putStrLn $ "hasControlChar: " ++ show hasControlChar
    putStrLn $ "notSpecialCase: " ++ show notSpecialCase
    putStrLn $ "noBraces: " ++ show noBraces
    putStrLn $ "control chars condition: " ++ show (hasControlChar && notSpecialCase && noBraces)
    
    -- Check if it's a single line
    let inputLines = lines input
    putStrLn $ "Number of lines: " ++ show (length inputLines)
    putStrLn $ "Lines: " ++ show inputLines
    
    -- Check if it has mixed indentation
    let hasTab = '\t' `elem` input
    let hasSpace = ' ' `elem` input
    let notAllSpace = not (all isSpace input)
    putStrLn $ "hasTab: " ++ show hasTab
    putStrLn $ "hasSpace: " ++ show hasSpace
    putStrLn $ "notAllSpace: " ++ show notAllSpace
    putStrLn $ "mixed indentation condition: " ++ show (hasTab && hasSpace && notAllSpace)
    
    let normalized = normalizeIndentation input
    putStrLn $ "Normalized: " ++ show normalized
    
    putStrLn $ "Expected: input (unchanged)"
    putStrLn $ "Test passes: " ++ show (normalized == input)