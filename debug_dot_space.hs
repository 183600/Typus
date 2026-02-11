import Utils

-- Test the " . " case
main :: IO ()
main = do
  let input = " ."
  putStrLn $ "Input: " ++ show input
  let result = normalizeIndentation input
  putStrLn $ "Output: " ++ show result