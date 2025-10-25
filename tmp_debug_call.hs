import Ownership

main :: IO ()
main = do
  let code = "take_value(data)  // Move is OK\n"
  let toks = lexAll code
  putStrLn "Tokens:"
  print toks
  let ast = parseProgram toks
  putStrLn "AST:"
  print ast
  let (errs, logs) = analyzeOwnershipDebug True code
  putStrLn "Logs:"
  mapM_ putStrLn logs
  putStrLn "Errors:"
  print errs
