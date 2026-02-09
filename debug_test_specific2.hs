import Utils
import Data.List (isInfixOf)

main :: IO ()
main = do
  putStrLn "Testing removeComments with specific cases:"
  
  -- Test case: " /* a */"
  putStrLn "\nTesting removeComments with \" /* a */\":"
  let input = "\" /* a */\""
  putStrLn $ "Input: " ++ show input
  let result = removeComments input
  putStrLn $ "Output: " ++ show result
  putStrLn $ "Expected: \"\\\"\\\"\\\"\" (which is \"\")"
  putStrLn $ "Test passes: " ++ show (result == "\"\"")
  
  -- Check if quotedStr is in result
  let quotedStr = "\""
  putStrLn $ "quotedStr \" in result: " ++ show (quotedStr `isInfixOf` result)