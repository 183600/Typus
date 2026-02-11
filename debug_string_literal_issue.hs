import qualified Utils as U

main :: IO ()
main = do
  let s = ""
  let escaped = "\"" ++ s ++ "\\\"\""
  putStrLn $ "Input string: " ++ show escaped
  putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral escaped)
  putStrLn $ "Expected: True"