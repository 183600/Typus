import Utils (isValidChar)
import Data.Char (ord)

-- Test cases based on the failing minimal char property
main :: IO ()
main = do
    putStrLn "Testing isValidChar with various characters:"
    
    -- Test with printable ASCII characters
    mapM_ testChar [' '..'~']
    
    -- Test with specific control characters
    testChar '\n'  -- newline
    testChar '\r'  -- carriage return
    testChar '\t'  -- tab
    testChar '\0'  -- null
    testChar '\DEL'  -- delete
    
    -- Test with some non-printable characters
    testChar '\1'   -- SOH
    testChar '\2'   -- STX
    testChar '\26'  -- SUB
    testChar '\127' -- DEL

  where
    testChar c = do
        let isValid = isValidChar c
        let ordC = ord c
        putStrLn $ "  Char '" ++ show c ++ "' (ord " ++ show ordC ++ "): " ++ show isValid