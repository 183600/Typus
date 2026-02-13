import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with multiline mixed..."
  
  -- Test with lines' = ["\n7"]
  let testInput = ["\n7"]
  let withMixed = map ("\t  " ++) testInput
  let normalized = U.normalizeIndentation (unlines withMixed)
  let normLines = lines normalized
  
  putStrLn $ "Input lines': " ++ show testInput
  putStrLn $ "With mixed: " ++ show withMixed
  putStrLn $ "Unlines withMixed: " ++ show (unlines withMixed)
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Normalized lines: " ++ show normLines
  putStrLn $ "Length of normalized lines: " ++ show (length normLines)
  putStrLn $ "Expected length: 1"
  putStrLn $ "Test passed: " ++ show (length normLines == 1)