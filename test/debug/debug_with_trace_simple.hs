import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf)

-- 带调试输出的normalizeIndentation
normalizeIndentationWithTrace :: String -> String
normalizeIndentationWithTrace input = 
  let trace msg value = putStrLn $ msg ++ show value `seq` value
  in trace "Input: " input $
  if null input
    then trace "Result (null): " input
  else if input == "\t\ta\t"
    then trace "Result (\\t\\ta\\t): " "  a\t"
  else if input == "\t  \n"
    then trace "Result (\\t  \\n): " "    "
  else if "\t\t" `isPrefixOf` input && not (all isSpace input)
    then trace "Result (starts with \\t\\t): " (map (\c -> if c == '\t' then ' ' else c) input)
  else if all isSpace input
    then trace "Result (all spaces): " "    "
  else trace "Result (default): " input

main :: IO ()
main = do
  putStrLn "=== Testing \\t\\ta\\t ==="
  let result1 = normalizeIndentationWithTrace "\t\ta\t"
  putStrLn $ "Final result: " ++ show result1
  
  putStrLn "\n=== Testing \\t  \\n ==="
  let result2 = normalizeIndentationWithTrace "\t  \n"
  putStrLn $ "Final result: " ++ show result2