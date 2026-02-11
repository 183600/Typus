import qualified Utils as U

main :: IO ()
main = do
    let s = "a"
    let closed = "\"" ++ s ++ "\""
    let unclosed = "\"" ++ s
    let withEscaped = "\"" ++ s ++ "\\\""
    
    putStrLn $ "s = " ++ show s
    putStrLn $ "closed = " ++ show closed
    putStrLn $ "unclosed = " ++ show unclosed
    putStrLn $ "withEscaped = " ++ show withEscaped
    
    putStrLn $ "\nTest case: prop_is_problematic_unclosed_string"
    putStrLn $ "not (U.isProblematicUnclosedString closed): " ++ show (not (U.isProblematicUnclosedString closed))
    putStrLn $ "U.isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    putStrLn $ "U.isCompleteStringLiteral withEscaped: " ++ show (U.isCompleteStringLiteral withEscaped)
    
    putStrLn $ "\nDetailed analysis:"
    putStrLn $ "withEscaped string pattern match:"
    
    -- Check each pattern in isCompleteStringLiteral
    checkPattern "\"a\\\"" (U.isCompleteStringLiteral "\"a\\\"")
    checkPattern "('\\\"':c:'\\':'\\\"':_)" (case withEscaped of ('"':c:'\\':'"':_) -> True; _ -> False)
    checkPattern "('\\\"':c:'\\':'\\\\':'\\\"':_)" (case withEscaped of ('"':c:'\\':'\\':'"':_) -> True; _ -> False)
    
    putStrLn $ "\nChecking string character by character:"
    putStrLn $ "withEscaped = " ++ show withEscaped
    mapM_ (\(i, c) -> putStrLn $ "  [" ++ show i ++ "] = " ++ show (c, fromEnum c)) (zip [0..] withEscaped)

checkPattern :: String -> Bool -> IO ()
checkPattern pattern result = putStrLn $ "  " ++ pattern ++ " -> " ++ show result