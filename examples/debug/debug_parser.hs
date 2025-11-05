import qualified Dependencies as Dep

main :: IO ()
main = do
  let input = "type Map<T, U> = func(T): U"
  putStrLn $ "Testing: " ++ input
  case Dep.runParser input of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right ast -> putStrLn $ "Success: " ++ show ast
