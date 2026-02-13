import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with empty string:"
  putStrLn $ "Result: " ++ show (U.normalizeIndentation "")
  
  putStrLn "\nTesting normalizeIndentation with ["\n\"]:"
  let testInput = "\n"
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Result: " ++ show (U.normalizeIndentation testInput)
  
  putStrLn "\nTesting normalizeIndentation with mixed \n:"
  let mixedInput = "\t  \t  \n  \t  "
  putStrLn $ "Input: " ++ show mixedInput
  putStrLn $ "Result: " ++ show (U.normalizeIndentation mixedInput)
  
  putStrLn "\nTesting normalizeIndentation with code block empty:"
  let codeBlockInput = ""
  putStrLn $ "Input: " ++ show codeBlockInput
  putStrLn $ "Result: " ++ show (U.normalizeIndentation codeBlockInput)
  
  putStrLn "\nTesting normalizeIndentation with nested empty:"
  let nestedInput = ""
  putStrLn $ "Input: " ++ show nestedInput
  putStrLn $ "Result: " ++ show (U.normalizeIndentation nestedInput)