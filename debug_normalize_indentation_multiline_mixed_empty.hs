import Utils (normalizeIndentation)

main :: IO ()
main = do
  let lines' = [""]
      withMixed = map ("\t  " ++) lines'
      normalized = normalizeIndentation (unlines withMixed)
      normLines = lines normalized
  putStrLn $ "lines': " ++ show lines'
  putStrLn $ "withMixed: " ++ show withMixed
  putStrLn $ "unlines withMixed: " ++ show (unlines withMixed)
  putStrLn $ "normalized: " ++ show normalized
  putStrLn $ "normLines: " ++ show normLines
  putStrLn $ "expected: \"\\t  \\n\""