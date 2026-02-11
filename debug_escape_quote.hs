import Utils

-- Test the failing cases
main :: IO ()
main = do
  -- Test prop_is_problematic_unclosed_escape_quote
  let input1 = "a"
  putStrLn $ "input1: " ++ show input1
  putStrLn $ "isProblematicUnclosedString input1: " ++ show (Utils.isProblematicUnclosedString input1)
  
  -- Test prop_is_problematic_unclosed_string
  let input2 = "a\""
  putStrLn $ "input2: " ++ show input2
  putStrLn $ "isProblematicUnclosedString input2: " ++ show (Utils.isProblematicUnclosedString input2)