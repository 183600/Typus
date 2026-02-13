import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing isCompleteStringLiteral..."
  
  -- Test with s = "a\"" (which is the string a")
  let s = "a\""
  let quoted = "\"" ++ s ++ "\""  -- This should be "\"a\"\""
  let incomplete = "\"" ++ s     -- This should be "\"a\""
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "s chars: " ++ concatMap (\c -> show c ++ " (" ++ show (fromEnum c) ++ ") ") s
  putStrLn $ "quoted: " ++ show quoted
  putStrLn $ "quoted chars: " ++ concatMap (\c -> show c ++ " (" ++ show (fromEnum c) ++ ") ") quoted
  putStrLn $ "incomplete: " ++ show incomplete
  putStrLn $ "incomplete chars: " ++ concatMap (\c -> show c ++ " (" ++ show (fromEnum c) ++ ") ") incomplete
  
  let quotedResult = U.isCompleteStringLiteral quoted
  let incompleteResult = U.isCompleteStringLiteral incomplete
  
  putStrLn $ "quoted is complete: " ++ show quotedResult
  putStrLn $ "incomplete is complete: " ++ show incompleteResult
  
  -- According to the test, quoted should be True and incomplete should be False
  let testPassed = quotedResult && not incompleteResult
  putStrLn $ "Test passed: " ++ show testPassed