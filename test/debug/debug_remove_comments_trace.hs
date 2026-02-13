import qualified Utils as U

-- A simple wrapper to add debug output
removeCommentsDebug :: String -> IO String
removeCommentsDebug input = 
  let result = U.removeComments input
  in if input == "//5\n"
     then do
       putStrLn ("DEBUG: Processing \"//5\\n\"")
       putStrLn $ "Length: " ++ show (length input)
       putStrLn $ "Take 2: " ++ show (take 2 input)
       putStrLn $ "Last: " ++ show (last input)
       putStrLn $ "Condition: " ++ show (length input == 4 && take 2 input == "//" && last input == '\n')
       return result
     else return result

main :: IO ()
main = do
  putStrLn "Testing removeComments..."
  
  -- Test with s = "5\n"
  let s = "5\n"
  let withSingle = "//" ++ s  -- This should be "//5\n"
  processed <- removeCommentsDebug withSingle
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "withSingle: " ++ show withSingle
  putStrLn $ "Processed: " ++ show processed
  
  -- According to the test, when s contains '\n', processed should equal s
  let expected = "5\n"
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Test passed: " ++ show (processed == expected)