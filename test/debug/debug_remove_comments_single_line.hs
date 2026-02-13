import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing removeComments..."
  
  -- Test with s = "5\n"
  let s = "5\n"
  let withSingle = "//" ++ s  -- This should be "//5\n"
  let processed = U.removeComments withSingle
  
  putStrLn $ "s: " ++ show s
  putStrLn $ "withSingle: " ++ show withSingle
  putStrLn $ "withSingle chars: " ++ concatMap (\c -> show c ++ " (" ++ show (fromEnum c) ++ ") ") withSingle
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Processed chars: " ++ concatMap (\c -> show c ++ " (" ++ show (fromEnum c) ++ ") ") processed
  
  -- According to the test, when s contains '\n', processed should equal s
  let expected = "5\n"
  putStrLn $ "Expected: " ++ show expected
  putStrLn $ "Test passed: " ++ show (processed == expected)