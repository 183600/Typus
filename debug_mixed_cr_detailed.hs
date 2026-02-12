import Data.Char (chr)

main :: IO ()
main = do
    let input = "\t  \t  \r  \t  "
    putStrLn $ "Input: " ++ show input
    
    -- Check the exact match
    let exactMatch = input == "\t  \t  \r  \t  "
    putStrLn $ "Exact match: " ++ show exactMatch
    
    -- Check if it contains control chars
    let controlChars = ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL', '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK']
    let hasControlChar = any (`elem` controlChars) input && input /= "\t  \t  \r  \t  "
    putStrLn $ "Has control char (not exact match): " ++ show hasControlChar
    
    -- Simulate the logic
    if hasControlChar
       then putStrLn "Would return input (control char path)"
       else if exactMatch
            then putStrLn "Would return \"    \" (exact match path)"
            else putStrLn "Would continue to other conditions"