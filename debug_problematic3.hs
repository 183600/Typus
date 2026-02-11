import Utils

main :: IO ()
main = do
  putStrLn $ "isProblematicUnclosedString \"\" = " ++ show (isProblematicUnclosedString "")