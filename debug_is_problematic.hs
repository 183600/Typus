import Utils

main :: IO ()
main = do
  let input = "a"
  putStrLn $ "input: " ++ show input
  putStrLn $ "isProblematicUnclosedString: " ++ show (isProblematicUnclosedString input)
  putStrLn $ "isCompleteStringLiteral: " ++ show (isCompleteStringLiteral input)