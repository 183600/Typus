import Data.Char (chr)

main :: IO ()
main = do
    let input = "\t  \t  \r  \t  "
    putStrLn $ "Input: " ++ show input
    
    -- Check if it contains \r
    let hasCR = '\r' `elem` input
    putStrLn $ "Contains '\\r': " ++ show hasCR
    
    -- Check if it contains any of the special control characters
    let controlChars = ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL', '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK']
    let hasControlChar = any (`elem` controlChars) input
    putStrLn $ "Contains any control char: " ++ show hasControlChar
    
    -- Check exact match
    let exactMatch = input == "\t  \t  \r  \t  "
    putStrLn $ "Exact match: " ++ show exactMatch