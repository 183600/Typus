import qualified Parser as P

main :: IO ()
main = do
  -- Test directive parsing
  let input1 = "//! ownership: on\npackage main\nfunc main() {}"
  putStrLn $ "Testing input1: " ++ input1
  case P.parseTypus input1 of
    Left err -> putStrLn $ "Parse failed: " ++ err
    Right parsed -> do
      putStrLn "Parse succeeded"
      putStrLn $ "File directives: " ++ show (P.tfDirectives parsed)
  
  let input2 = "//! ownership=true\npackage main\nfunc main() {}"
  putStrLn $ "\nTesting input2: " ++ input2
  case P.parseTypus input2 of
    Left err -> putStrLn $ "Parse failed: " ++ err
    Right parsed -> do
      putStrLn "Parse succeeded"
      putStrLn $ "File directives: " ++ show (P.tfDirectives parsed)
  
  let input3 = "//! ownership\npackage main\nfunc main() {}"
  putStrLn $ "\nTesting input3: " ++ input3
  case P.parseTypus input3 of
    Left err -> putStrLn $ "Parse failed: " ++ err
    Right parsed -> do
      putStrLn "Parse succeeded"
      putStrLn $ "File directives: " ++ show (P.tfDirectives parsed)