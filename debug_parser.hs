import Parser

main :: IO ()
main = do
  let codeWithBlockDirectives = "```typus\n// @ownership: true\n// @constraints: true\nlet x = 42\n```"
      result = parseTypus codeWithBlockDirectives
  case result of
    Left err -> putStrLn $ "Failed to parse block directives: " ++ show err
    Right typusFile -> do
      let blocks = tfBlocks typusFile
          block = head blocks
          directives = cbDirectives block
          ownership = bdOwnership directives
          constraints = bdConstraints directives
      putStrLn $ "Number of blocks: " ++ show (length blocks)
      putStrLn $ "Block content: " ++ show (cbContent block)
      putStrLn $ "Directives: " ++ show directives
      putStrLn $ "Ownership: " ++ show ownership
      putStrLn $ "Constraints: " ++ show constraints