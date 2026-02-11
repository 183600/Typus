import Utils

main :: IO ()
main = do
  let lines' = [""]
  let code = unlines lines'
  let processed = removeLineComments code
  putStrLn $ "lines' = " ++ show lines'
  putStrLn $ "code = " ++ show code
  putStrLn $ "processed = " ++ show processed
  putStrLn $ "expected = " ++ show ""