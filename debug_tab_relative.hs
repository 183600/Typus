import Utils

main :: IO ()
main = do
  let s = "\t"
      normalized = normalizeIndentation s
      expected = "    "
  putStrLn $ "s = " ++ show s
  putStrLn $ "normalized = " ++ show normalized
  putStrLn $ "expected = " ++ show expected