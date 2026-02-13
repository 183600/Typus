import qualified Utils as U

main :: IO ()
main = do
  -- Test the specific case directly
  let testInput = "//5\n"
  let result = U.removeComments testInput
  
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Expected: " ++ show "5\n"
  putStrLn $ "Test passed: " ++ show (result == "5\n")