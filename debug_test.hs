import Utils

main :: IO ()
main = do
  putStrLn $ "isCompleteStringLiteral \"\\\"\" = " ++ show (isCompleteStringLiteral "\"")
  putStrLn $ "isCompleteStringLiteral \"\\\\\"\" = " ++ show (isCompleteStringLiteral "\\\"")
  putStrLn $ "isProblematicUnclosedString \"\\\"\" = " ++ show (isProblematicUnclosedString "\"")
  putStrLn $ "removeComments \"\\\"\" = " ++ show (removeComments "\"")