import qualified Utils as U

main :: IO ()
main = do
  let s = "\""
  let escaped = "\"" ++ s ++ "\\\"\""
  putStrLn $ "s = " ++ show s
  putStrLn $ "escaped = " ++ show escaped
  putStrLn $ "isCompleteStringLiteral: " ++ show (U.isCompleteStringLiteral escaped)
  putStrLn $ "Expected: True"