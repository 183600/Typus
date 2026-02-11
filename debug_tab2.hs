import Utils

-- Test the failing case
main :: IO ()
main = do
  let s = "\t"
  let withEmpty = s ++ "\n\n"
  let normalized = Utils.normalizeIndentation withEmpty
  putStrLn $ "s: " ++ show s
  putStrLn $ "withEmpty: " ++ show withEmpty
  putStrLn $ "normalized: " ++ show normalized