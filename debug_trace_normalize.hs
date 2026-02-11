import Utils

-- Debug the normalizeIndentation function
main :: IO ()
main = do
  let input = "\n\n"
  putStrLn $ "input: " ++ show input
  let result = Utils.normalizeIndentation input
  putStrLn $ "result: " ++ show result