import Utils

main :: IO ()
main = do
  -- Test isProblematicUnclosedString
  putStrLn $ "isProblematicUnclosedString \"\"\" = " ++ show (isProblematicUnclosedString "\"")
  putStrLn $ "isProblematicUnclosedString \"\"\"\" = " ++ show (isProblematicUnclosedString "\"\"")
  putStrLn $ "isProblematicUnclosedString \"\"\\\"\" = " ++ show (isProblematicUnclosedString "\"\\")
  putStrLn $ "isProblematicUnclosedString \"\"\\\"\"\" = " ++ show (isProblematicUnclosedString "\"\\\"")