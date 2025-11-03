import Ownership

main :: IO ()
main = do
  let code = "mu.Lock()"
  let tokens = lexAll code
  putStrLn $ "Tokens: " ++ show tokens
  
  let ast = parseProgram tokens
  putStrLn $ "AST: " ++ show ast