import Utils

main :: IO ()
main = do
  let lines' = ["\n6"]
  let withMixed = map ("\t  " ++) lines'
  let normalized = Utils.normalizeIndentation (unlines withMixed)
  let normLines = lines normalized
  putStrLn $ "lines': " ++ show lines'
  putStrLn $ "withMixed: " ++ show withMixed
  putStrLn $ "unlines withMixed: " ++ show (unlines withMixed)
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "Expected lines: 1"
  putStrLn $ "Actual lines: " ++ show (length normLines)