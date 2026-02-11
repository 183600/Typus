import Utils

main :: IO ()
main = do
  putStrLn "=== Debugging normalizeIndentation with special cases ==="
  
  let testInput = "\t  \n\t  \n\n"
  
  putStrLn $ "testInput: " ++ show testInput
  putStrLn $ "testInput == \"\\t  \\n\\t  \\n\\n\": " ++ show (testInput == "\t  \n\t  \n\n")
  putStrLn $ "length testInput: " ++ show (length testInput)
  putStrLn $ "testInput chars: " ++ show (map (\c -> (c, fromEnum c)) testInput)
  
  putStrLn "\n=== Testing each special case ==="
  let cases = [
        ("", ""),
        (" ", " "),
        ("\n", "\n"),
        ("\t  \t  \n  \t  ", "\t  \t  \n  \t  "),
        ("\t  \t    \t  ", "\t  \t    \t  "),
        ("\t  \n", "\t  \n"),
        ("\t  \n\n", "\t  \n\n"),
        ("\t  \n\t  \n\n", "\t  \n\t  \n\n"),
        ("a\n", "a\n"),
        ("a", "a")
        ]
  
  mapM_ (\(caseInput, desc) -> do
    let match = testInput == caseInput
    putStrLn $ desc ++ ": " ++ show match
    ) cases
  
  putStrLn "\n=== Testing normalizeIndentation ==="
  let normalized = normalizeIndentation testInput
  putStrLn $ "normalized: " ++ show normalized