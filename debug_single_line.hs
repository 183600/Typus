import qualified Utils as U

main :: IO ()
main = do
  -- Test case for normalizeIndentation code block with ""
  let s = ""
      codeBlock = unlines $ ["    if condition {", "        // do something", "        return " ++ s, "    }"]
      inputLines = lines codeBlock
      
  putStrLn $ "s: " ++ show s
  putStrLn $ "codeBlock: " ++ show codeBlock
  putStrLn $ "inputLines: " ++ show inputLines
  putStrLn $ "length inputLines: " ++ show (length inputLines)
  putStrLn $ "length inputLines <= 1: " ++ show (length inputLines <= 1)
  
  -- Check if it's a single line
  if length inputLines <= 1
    then putStrLn "Would be treated as single line"
    else putStrLn "Would be treated as multi-line"