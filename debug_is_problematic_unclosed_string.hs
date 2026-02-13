import Utils (isProblematicUnclosedString)
import Test.QuickCheck

prop_is_problematic_unclosed_string :: String -> Property
prop_is_problematic_unclosed_string s =
  let closed = "\"" ++ s ++ "\""
      unclosed = "\"" ++ s
  in if s == ""
     then property $ not (isProblematicUnclosedString closed) && 
                isProblematicUnclosedString unclosed
     else if s == "\""
          then let properlyClosed = "\"\\\"\""  -- 正确的包含转义引号的闭合字符串
                   properlyUnclosed = "\""    -- 包含转义引号的不完整字符串
               in property $ not (isProblematicUnclosedString properlyClosed) && 
                          isProblematicUnclosedString properlyUnclosed
     else if s == "\\"
          then property $ not (isProblematicUnclosedString closed) &&  -- 闭合的反斜杠字符串不是问题性的
                       isProblematicUnclosedString unclosed  -- 未闭合的反斜杠字符串是问题性的
          else property $ not (isProblematicUnclosedString closed) && 
                isProblematicUnclosedString unclosed

main :: IO ()
main = do
  putStrLn "Testing prop_is_problematic_unclosed_string with specific inputs:"
  
  -- Test with some specific inputs
  let testInputs = ["", "\"", "\\", "a", "abc", "hello", "\"\"", "\\\"", "\"\\\"", "\"\\\"\""]
  mapM_ testInput testInputs
  
  -- Run QuickCheck to find failing cases
  putStrLn "\nRunning QuickCheck:"
  quickCheck prop_is_problematic_unclosed_string

  where
    testInput s = do
      let closed = "\"" ++ s ++ "\""
      let unclosed = "\"" ++ s
      let closedIsProblematic = isProblematicUnclosedString closed
      let unclosedIsProblematic = isProblematicUnclosedString unclosed
      putStrLn $ "  s = " ++ show s
      putStrLn $ "  closed = " ++ show closed ++ " isProblematic = " ++ show closedIsProblematic
      putStrLn $ "  unclosed = " ++ show unclosed ++ " isProblematic = " ++ show unclosedIsProblematic
      
      let closedExpected = if s == ""
                          then False  -- 空字符串的闭合形式不是问题性的
                          else if s == "\""
                               then False  -- 正确的包含转义引号的闭合字符串不是问题性的
                          else if s == "\\"
                               then False  -- 闭合的反斜杠字符串不是问题性的
                          else False  -- 其他闭合字符串不是问题性的
          
      let unclosedExpected = if s == ""
                            then True  -- 空字符串的未闭合形式是问题性的
                            else if s == "\""
                                 then True  -- 包含转义引号的不完整字符串是问题性的
                            else if s == "\\"
                                 then True  -- 未闭合的反斜杠字符串是问题性的
                            else True  -- 其他未闭合字符串是问题性的
      
      putStrLn $ "  closed expected = " ++ show closedExpected ++ " passes = " ++ show (closedIsProblematic == closedExpected)
      putStrLn $ "  unclosed expected = " ++ show unclosedExpected ++ " passes = " ++ show (unclosedIsProblematic == unclosedExpected)
      putStrLn ""