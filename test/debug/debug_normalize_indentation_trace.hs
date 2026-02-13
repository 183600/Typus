import qualified Utils as U

-- A simple wrapper to add debug output
normalizeIndentationDebug :: String -> IO String
normalizeIndentationDebug input = 
  let result = U.normalizeIndentation input
  in if input == "\t  \n\n"
     then putStrLn ("DEBUG: Processing \"\\t  \\n\\n\"") >> return result
     else return result

main :: IO ()
main = do
  putStrLn "Testing normalizeIndentation with multiline mixed..."
  
  -- Test with lines' = ["\n"]
  let testInput = ["\n"]
  let withMixed = map ("\t  " ++) testInput
  let inputStr = unlines withMixed
  normalized <- normalizeIndentationDebug inputStr
  let normLines = lines normalized
  
  putStrLn $ "Input lines': " ++ show testInput
  putStrLn $ "With mixed: " ++ show withMixed
  putStrLn $ "Unlines withMixed: " ++ show inputStr
  putStrLn $ "Normalized: " ++ show normalized
  putStrLn $ "Expected: \"\\n\""
  putStrLn $ "Test passed: " ++ show (normalized == "\n")