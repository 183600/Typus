import qualified Utils as U

main :: IO ()
main = do
  let s = "a"
  putStrLn $ "Input s: " ++ show s
  
  let quoted = "\"" ++ s ++ "\""
  putStrLn $ "quoted: " ++ show quoted
  putStrLn $ "isCompleteStringLiteral quoted: " ++ show (U.isCompleteStringLiteral quoted)
  
  let incomplete = "\"" ++ s
  putStrLn $ "incomplete: " ++ show incomplete
  putStrLn $ "isCompleteStringLiteral incomplete: " ++ show (U.isCompleteStringLiteral incomplete)
  
  let endsWithEscapedQuote = not (null s) && length s >= 2 && drop (length s - 2) s == "\\\""
  putStrLn $ "endsWithEscapedQuote: " ++ show endsWithEscapedQuote
  
  putStrLn $ "\nTest expects:"
  putStrLn $ "  isCompleteStringLiteral quoted: True"
  putStrLn $ "  isCompleteStringLiteral incomplete: False"