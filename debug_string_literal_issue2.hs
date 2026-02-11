import qualified Utils as U

main :: IO ()
main = do
  let s = "\""
  putStrLn $ "Input string: " ++ show s
  putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral s)
  putStrLn $ "Expected: False"