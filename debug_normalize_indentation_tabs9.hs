import Utils
import Data.Char (chr, isSpace, ord)
import Data.List (isPrefixOf)

-- Create a wrapper to trace the execution
traceNormalizeIndentation :: String -> IO String
traceNormalizeIndentation input = do
    putStrLn $ "=== normalizeIndentation called with: " ++ show input
    
    -- Check early conditions
    let cond1 = null input
    putStrLn $ "null input: " ++ show cond1
    
    let cond2 = input == " "
    putStrLn $ "input == \" \": " ++ show cond2
    
    let cond3 = length input == 1 && not (isSpace (head input))
    putStrLn $ "single non-space: " ++ show cond3
    
    let cond4 = "\t\t" `isPrefixOf` input && last input == '\t'
    putStrLn $ "\"\\t\\t\" prefix and \"\\t\" suffix: " ++ show cond4
    
    if cond4
       then do
           let middle = drop 2 (init input)
           putStrLn $ "middle: " ++ show middle
           let isControlChar c = ord c < 32 || c == '\DEL'
           let hasControl = any isControlChar middle
           putStrLn $ "has control: " ++ show hasControl
           let isSingleSpace = middle == " "
           putStrLn $ "is single space: " ++ show isSingleSpace
           if hasControl
              then putStrLn "Returning input (control char)" >> return input
              else if isSingleSpace
                   then putStrLn "Returning input (single space)" >> return input
                   else putStrLn ("Converting: " ++ show ("  " ++ middle ++ "\t")) >> return ("  " ++ middle ++ "\t")
       else do
           let result = normalizeIndentation input
           putStrLn $ "Final result: " ++ show result
           return result

main :: IO ()
main = do
    let input = [chr 9, chr 9, ' ', chr 9]  -- \t\t \t
    result <- traceNormalizeIndentation input
    putStrLn $ "=== Final result: " ++ show result