import qualified Utils as U

main :: IO ()
main = do
  let testInput = "\""
      withSlash = "\"" ++ testInput ++ "// not comment\""
      processed = U.removeLineComments withSlash
  putStrLn $ "Input: " ++ show testInput
  putStrLn $ "With slash: " ++ show withSlash
  putStrLn $ "Processed: " ++ show processed
  putStrLn $ "Expected: " ++ show "\"// not comment\""
  putStrLn $ "Test passed: " ++ show (processed == "\"// not comment\"")