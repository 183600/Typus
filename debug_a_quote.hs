import Utils

-- Test the other failing case
main :: IO ()
main = do
  let input = "a\""
  putStrLn $ "input: " ++ show input
  putStrLn $ "isProblematicUnclosedString input: " ++ show (Utils.isProblematicUnclosedString input)