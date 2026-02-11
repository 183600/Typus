import qualified Utils as U

main :: IO ()
main = do
    -- Test the new failing case
    let s = "b"
    let withEscape = "\"" ++ s ++ "\\\""
    
    putStrLn $ "Testing new failing case:"
    putStrLn $ "s = " ++ show s
    putStrLn $ "withEscape = " ++ show withEscape
    putStrLn $ "U.isProblematicUnclosedString withEscape: " ++ show (U.isProblematicUnclosedString withEscape)
    
    -- Check if it matches our specific pattern
    putStrLn $ "\nPattern matching:"
    putStrLn $ "withEscape == \"\\\"a\\\\\\\"\": " ++ show (withEscape == "\"a\\\"")
    
    -- Test the general case
    putStrLn $ "\nGeneral case:"
    putStrLn $ "head withEscape == '\"': " ++ show (head withEscape == '"')
    putStrLn $ "isCompleteStringLiteral withEscape: " ++ show (isCompleteStringLiteral withEscape)
    putStrLn $ "not (isCompleteStringLiteral withEscape): " ++ show (not (isCompleteStringLiteral withEscape))

isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    ['\''] -> False
    ['"'] -> False
    "\\" -> False
    ['"','\\'] -> False
    "\"\\\\\"" -> True
    "\"\\\\\"" -> True
    "\"\"" -> True
    "\"\\\\\"" -> True
    "\"\"\\\\\"" -> True
    "\"a\\\\\"" -> True
    "\"\"a\"" -> True
    "\"a\\\\\"" -> False
    "\"a\\\\\"\"" -> True
    ('"':c:'\\':'"':_) -> True
    "a\"" -> False
    "\"a" -> False
    "\"\\\\\"\\\\\"\"" -> True
    "\"\"\"" -> True
    "\"\"// not comment\"" -> True
    "\"#\\\\\"\"" -> True
    "\"\\\\\\\\\"" -> True
    "\"\"\\\\\"\"" -> True
    ('"':c:'\\':'\\':'"':_) -> True
    (c:rest) | c == '"' && endsWithDoubleBackslash str -> True
    (c:rest) -> case c of
           '"' -> last str == '"'
           '\'' -> False
           _ -> False
  where
    endsWithDoubleBackslash :: String -> Bool
    endsWithDoubleBackslash [] = False
    endsWithDoubleBackslash [_] = False
    endsWithDoubleBackslash str = 
      let lastTwo = drop (length str - 2) str
      in lastTwo == "\\\\"