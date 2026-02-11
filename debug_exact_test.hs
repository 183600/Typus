import Utils

-- Test the exact test case
main :: IO ()
main = do
  let s = "a"
  let withEscape = "\"" ++ s ++ "\\\""
  putStrLn $ "s: " ++ show s
  putStrLn $ "withEscape: " ++ show withEscape
  putStrLn $ "isProblematicUnclosedString withEscape: " ++ show (Utils.isProblematicUnclosedString withEscape)