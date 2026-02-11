import Utils

main :: IO ()
main = do
  putStrLn "=== Testing normalizeIndentation ==="
  
  -- Test case 1: prop_normalize_indentation_multiline_mixed failure
  let lines1 = ["", "\n"]
  let withMixed1 = map ("\t  " ++) lines1
  let normalized1 = normalizeIndentation (unlines withMixed1)
  let normLines1 = lines normalized1
  putStrLn $ "Input lines: " ++ show lines1
  putStrLn $ "With mixed: " ++ show withMixed1
  putStrLn $ "Normalized: " ++ show normalized1
  putStrLn $ "Norm lines: " ++ show normLines1
  putStrLn $ "Expected length: 2, Actual length: " ++ show (length normLines1)
  putStrLn ""
  
  -- Test case 2: isCompleteStringLiteral with escape backslash
  let testStr2 = "\\"
  let withBackslash2 = "\"" ++ testStr2 ++ "\\\\\""
  let result2 = isCompleteStringLiteral withBackslash2
  putStrLn $ "Input: " ++ show testStr2
  putStrLn $ "With backslash: " ++ show withBackslash2
  putStrLn $ "isCompleteStringLiteral result: " ++ show result2
  putStrLn ""
  
  -- Test case 3: removeLineComments with newline
  let testStr3 = "\n"
  let result3 = removeLineComments testStr3
  putStrLn $ "Input: " ++ show testStr3
  putStrLn $ "removeLineComments result: " ++ show result3
  putStrLn ""
  
  -- Test case 4: normalizeIndentation with empty string
  let testStr4 = ""
  let result4 = normalizeIndentation testStr4
  putStrLn $ "Input: " ++ show testStr4
  putStrLn $ "normalizeIndentation result: " ++ show result4
  putStrLn ""