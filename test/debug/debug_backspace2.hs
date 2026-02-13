import qualified Utils as U
import Data.Char (isPrint, ord)
import Data.List (isPrefixOf)

endsWith :: String -> Char -> Bool
endsWith [] _ = False
endsWith s c = case reverse s of
                [] -> False
                (x:_) -> x == c

main :: IO ()
main = do
    let input = "\b"
    let withTabs = "\t\t" ++ input ++ "\t"
    
    putStrLn $ "Input: " ++ show input
    putStrLn $ "With tabs: " ++ show withTabs
    
    -- 检查各种条件
    putStrLn $ "startsWithTwoTabs: " ++ show ("\t\t" `isPrefixOf` withTabs)
    putStrLn $ "endsWithTab: " ++ show (endsWith withTabs '\t')
    putStrLn $ "containsNonPrintable: " ++ show (any (\c -> not (isPrint c) && c `notElem` "\n\r\t " && ord c < 128 && c /= '\f' && c /= '\v') withTabs)
    putStrLn $ "containsControlChars: " ++ show (any (\c -> c `elem` ['\f', '\v', '\b', '\a', '\BEL', '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK', '\SYN', '\ETB', '\CAN', '\EM', '\SUB', '\ESC', '\FS', '\GS', '\RS', '\US', '\DEL']) withTabs)
    putStrLn $ "containsTab: " ++ show ('\t' `elem` withTabs)
    
    let normalized = U.normalizeIndentation withTabs
    putStrLn $ "Normalized: " ++ show normalized
    putStrLn $ "Expected: " ++ show withTabs
    putStrLn $ "Test passes: " ++ show (normalized == withTabs)