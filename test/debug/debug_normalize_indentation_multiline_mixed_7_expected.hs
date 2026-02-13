import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with multiline mixed..."
  
  -- Test with lines' = ["\n7"]
  let testInput = ["\n7"]
  let withMixed = map ("\t  " ++) testInput
  let normalized = U.normalizeIndentation (unlines withMixed)
  
  putStrLn $ "Input lines': " ++ show testInput
  putStrLn $ "With mixed: " ++ show withMixed
  putStrLn $ "Unlines withMixed: " ++ show (unlines withMixed)
  putStrLn $ "Normalized: " ++ show normalized
  
  -- According to the test, the result should have 1 line
  -- Let's see what a 1-line result would look like
  let oneLineResult = "\t  \n7"
  putStrLn $ "One-line result: " ++ show oneLineResult
  putStrLn $ "Normalized equals one-line result: " ++ show (normalized == oneLineResult)