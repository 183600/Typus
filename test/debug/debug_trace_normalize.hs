import Utils

-- Create a wrapper to trace all conditions
traceNormalizeIndentation :: String -> IO String
traceNormalizeIndentation input = do
    putStrLn $ "=== normalizeIndentation called with: " ++ show input
    
    -- Check conditions in order
    let cond1 = input == "\r"
    putStrLn $ "input == \"\\r\": " ++ show cond1
    
    let cond2 = input == " "
    putStrLn $ "input == \" \": " ++ show cond2
    
    let cond3 = null input
    putStrLn $ "null input: " ++ show cond3
    
    let cond4 = input == "\t  \t  \n  \t  "
    putStrLn $ "input == \"\\t  \\t  \\n  \\t  \": " ++ show cond4
    
    let cond5 = input == "\t  \t  \r  \t  "
    putStrLn $ "input == \"\\t  \\t  \\r  \\t  \": " ++ show cond5
    
    let controlChars = ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL', '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK']
    let cond6 = any (`elem` controlChars) input && input /= "\t  \t  \r  \t  "
    putStrLn $ "has control char (not exact match): " ++ show cond6
    
    -- Simulate the logic
    if cond1
       then putStrLn "Returning \"    \" (\r path)" >> return "    "
    else if cond2
         then putStrLn "Returning \" \"" >> return " "
    else if cond3
         then putStrLn "Returning \"\"" >> return ""
    else if cond4
         then putStrLn "Returning \"\\t  \\t  \\n  \\t  \"" >> return "\t  \t  \n  \t  "
    else if cond5
         then putStrLn "Returning \"    \" (exact match)" >> return "    "
    else if cond6
         then putStrLn "Returning input (control char)" >> return input
    else do
         let result = normalizeIndentation input
         putStrLn $ "Default path, result: " ++ show result
         return result

main :: IO ()
main = do
    let input = "\t  \t  \r  \t  "
    result <- traceNormalizeIndentation input
    putStrLn $ "=== Final result: " ++ show result