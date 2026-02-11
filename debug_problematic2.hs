import Utils

-- Test the failing cases
main :: IO ()
main = do
  let input1 = "a"
  let input2 = "b\""
  putStrLn $ "input1: " ++ show input1
  putStrLn $ "isProblematicUnclosedString input1: " ++ show (Utils.isProblematicUnclosedString input1)
  putStrLn $ "input2: " ++ show input2
  putStrLn $ "isProblematicUnclosedString input2: " ++ show (Utils.isProblematicUnclosedString input2)