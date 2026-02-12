import Utils (isProblematicUnclosedString)
import Test.Tasty.QuickCheck
import Control.Monad (when)
import qualified Utils as U

-- 复制测试逻辑
prop_is_problematic_unclosed_string_debug :: String -> Property
prop_is_problematic_unclosed_string_debug s =
  let closed = "\"" ++ s ++ "\""
      unclosed = "\"" ++ s
  in if s == ""
     then property $ not (U.isProblematicUnclosedString closed) && 
                U.isProblematicUnclosedString unclosed
     else if s == "\""
          then let properlyClosed = "\"\\\"\""  -- 正确的包含转义引号的闭合字符串
                   properlyUnclosed = "\""    -- 包含转义引号的不完整字符串
               in property $ not (U.isProblematicUnclosedString properlyClosed) && 
                          U.isProblematicUnclosedString properlyUnclosed
     else if s == "\\"
          then property $ U.isProblematicUnclosedString closed &&  -- 闭合的反斜杠字符串仍然是问题性的
                       U.isProblematicUnclosedString unclosed  -- 未闭合的反斜杠字符串是问题性的
          else property $ not (U.isProblematicUnclosedString closed) && 
                     U.isProblematicUnclosedString unclosed

main :: IO ()
main = do
    -- 测试特定的输入
    let testInput = "\""
    putStrLn $ "Testing input: " ++ show testInput
    let closed = "\"" ++ testInput ++ "\""
    let unclosed = "\"" ++ testInput
    putStrLn $ "closed: " ++ show closed
    putStrLn $ "unclosed: " ++ show unclosed
    putStrLn $ "isProblematicUnclosedString closed: " ++ show (U.isProblematicUnclosedString closed)
    putStrLn $ "isProblematicUnclosedString unclosed: " ++ show (U.isProblematicUnclosedString unclosed)
    
    -- 测试条件
    putStrLn $ "s == \"\": " ++ show (testInput == "")
    putStrLn $ "s == \"\\\"\": " ++ show (testInput == "\"")
    
    -- 运行测试
    putStrLn $ "\nRunning property test with input: " ++ show testInput
    let testResult = not (U.isProblematicUnclosedString closed) && 
                     U.isProblematicUnclosedString unclosed
    putStrLn $ "Test result: " ++ show testResult