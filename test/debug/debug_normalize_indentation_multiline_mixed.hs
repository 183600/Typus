import qualified Utils as U

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with multiline mixed..."
  
  -- Test with lines' = ["\n"]
  let testInput = ["\n"]
  let withMixed = map ("\t  " ++) testInput
  let normalized = U.normalizeIndentation (unlines withMixed)
  let normLines = lines normalized
  
  putStrLn $ "Input lines': " ++ show testInput
  putStrLn $ "With mixed: " ++ show withMixed
  putStrLn $ "Unlines withMixed: " ++ show (unlines withMixed)
  putStrLn $ "Unlines withMixed chars: " ++ concatMap (\c -> show (fromEnum c) ++ " ") (unlines withMixed)
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Normalized lines: " ++ show normLines
  putStrLn $ "Expected: \"\\n\""
  putStrLn $ "Test passed: " ++ show (normalized == "\n")