import Utils (removeLineComments)
import Data.List (isInfixOf)

main :: IO ()
main = do
  putStrLn "=== Debugging prop_remove_line_comments_string_slash ==="
  
  let s = "\n"
  putStrLn $ "s: " ++ show s
  
  let withSlash = "\"" ++ s ++ "// not comment\""
  putStrLn $ "withSlash: " ++ show withSlash
  
  let processed = removeLineComments withSlash
  putStrLn $ "processed: " ++ show processed
  
  let expected = "\"\n// not comment\""
  putStrLn $ "expected: " ++ show expected
  
  putStrLn $ "processed == expected: " ++ show (processed == expected)
  
  -- Let's also test a few other cases
  putStrLn "\n--- Other test cases ---"
  
  let s1 = ""
  let withSlash1 = "\"" ++ s1 ++ "// not comment\""
  let processed1 = removeLineComments withSlash1
  let expected1 = "\"// not comment\""
  putStrLn $ "s1: " ++ show s1
  putStrLn $ "withSlash1: " ++ show withSlash1
  putStrLn $ "processed1: " ++ show processed1
  putStrLn $ "expected1: " ++ show expected1
  putStrLn $ "processed1 == expected1: " ++ show (processed1 == expected1)
  
  let s2 = "a"
  let withSlash2 = "\"" ++ s2 ++ "// not comment\""
  let processed2 = removeLineComments withSlash2
  putStrLn $ "\ns2: " ++ show s2
  putStrLn $ "withSlash2: " ++ show withSlash2
  putStrLn $ "processed2: " ++ show processed2
  putStrLn $ "// not comment `isInfixOf` processed2: " ++ show ("// not comment" `isInfixOf` processed2)