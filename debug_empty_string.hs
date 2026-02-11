import Utils

-- Test the empty string case
main :: IO ()
main = do
  let s = ""
  let withEmpty = s ++ "\n\n"
  putStrLn $ "s: " ++ show s
  putStrLn $ "withEmpty: " ++ show withEmpty
  let normalized = normalizeIndentation withEmpty
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "expected: " ++ show "    "
  putStrLn $ "matches: " ++ show (normalized == "    ")
  putStrLn ""