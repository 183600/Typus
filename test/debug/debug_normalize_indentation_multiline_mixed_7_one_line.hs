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
  
  -- Check what a true 1-line result would be (no newline in the middle)
  let oneLineResult = "\t  7"  -- Replace \n with nothing
  putStrLn $ "True one-line result: " ++ show oneLineResult
  putStrLn $ "Normalized equals true one-line result: " ++ show (normalized == oneLineResult)
  
  -- Check lines count
  let normLines = lines normalized
  putStrLn $ "Normalized lines: " ++ show normLines
  putStrLn $ "Length of normalized lines: " ++ show (length normLines)
