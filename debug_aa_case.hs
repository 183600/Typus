import qualified Utils as U

main :: IO ()
main = do
    -- Test the failing case
    let s = "aa"
    let withEscape = "\"" ++ s ++ "\\\""
    
    putStrLn $ "Testing failing case:"
    putStrLn $ "s = " ++ show s
    putStrLn $ "withEscape = " ++ show withEscape
    putStrLn $ "U.isProblematicUnclosedString withEscape: " ++ show (U.isProblematicUnclosedString withEscape)
    
    -- Check pattern matching
    putStrLn $ "\nPattern matching:"
    case withEscape of
      ('"':_:'\\':'"':_) -> putStrLn $ "Matches pattern: True"
      _ -> putStrLn $ "Matches pattern: False"
    
    -- Check general case
    putStrLn $ "\nGeneral case:"
    putStrLn $ "head withEscape == '\"': " ++ show (head withEscape == '"')
    putStrLn $ "isCompleteStringLiteral withEscape: " ++ show (U.isCompleteStringLiteral withEscape)
    putStrLn $ "not (isCompleteStringLiteral withEscape): " ++ show (not (U.isCompleteStringLiteral withEscape))