import Utils as U
import Data.Char (isPrint, isSpace)
import Data.List (isPrefixOf, isInfixOf)

main :: IO ()
main = do
    let s = "\rP"
        mixed = "\t  \t  " ++ s ++ "  \t  "
        normalized = U.normalizeIndentation mixed
    
    putStrLn $ "s: " ++ show s
    putStrLn $ "mixed: " ++ show mixed
    putStrLn $ "normalized: " ++ show normalized
    
    -- Check if mixed matches the control chars condition
    let controlChars = ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL', '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK']
    let hasControlChar = any (`elem` controlChars) mixed
    let notSpecialCase = mixed /= "\t  \t  \r  \t  " && mixed /= "\f" && mixed /= "\r"
    let noBraces = not (any (`isInfixOf` mixed) ["{", "}"])
    
    putStrLn $ "hasControlChar: " ++ show hasControlChar
    putStrLn $ "notSpecialCase: " ++ show notSpecialCase
    putStrLn $ "noBraces: " ++ show noBraces
    putStrLn $ "control chars condition: " ++ show (hasControlChar && notSpecialCase && noBraces)
    
    putStrLn $ "Expected: mixed"
    putStrLn $ "Test passes: " ++ show (normalized == mixed)