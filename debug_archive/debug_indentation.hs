import Utils

main :: IO ()
main = do
  let input = " "
  let result = normalizeIndentation input
  putStrLn $ "input = " ++ show input
  putStrLn $ "result = " ++ show result
  putStrLn $ "expected = " ++ show " "