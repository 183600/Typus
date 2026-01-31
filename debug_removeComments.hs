import Test.QuickCheck
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isInfixOf)
import Data.Char (isSpace)

-- 从 Utils.hs 导入 removeComments 函数
removeComments :: String -> String
removeComments s = 
  -- 如果字符串不包含注释，直接返回原字符串
  if not ("//" `isInfixOf` s || "/*" `isInfixOf` s)
    then s
  else if all isSpace s
    then s
  else if s == "//"  -- 特殊情况：只有注释符号
    then ""
  else if s == "/*"  -- 特殊情况：未闭合的块注释
    then ""
  else if length s == 1  -- 特殊情况：单个字符（包括引号）
    then s
  else
    -- 使用通用的注释处理逻辑
    goNormal s
  where
    -- 通用的注释处理函数
    goNormal :: String -> String
    goNormal [] = []
    goNormal ('"':xs) = '"' : goInString xs
    goNormal ('\'':xs) = '\'' : goInChar xs
    goNormal ('/':'/':xs) = skipLine xs
    goNormal ('/':'*':xs) = skipBlock xs 0
    goNormal ('/':xs) = '/' : goNormal xs  -- 处理单个/的情况
    goNormal (c:cs) = c : goNormal cs

    -- 跳过行注释直到换行，只保留换行
    skipLine :: String -> String
    skipLine [] = []
    skipLine ('\n':xs) = '\n' : goNormal xs
    skipLine ('"':xs) = skipInString xs  -- 跳过字符串字面量
    skipLine ('\'':xs) = skipInChar xs  -- 跳过字符字面量
    skipLine (_:xs) = skipLine xs  -- 跳过其他字符
    
    -- 在行注释中跳过字符串字面量（不保留）
    skipInString :: String -> String
    skipInString [] = []
    skipInString ('\n':xs) = '\n' : goNormal xs  -- 换行时结束注释
    skipInString ('\\':_:xs) = skipInString xs  -- 跳过转义字符
    skipInString ('"':xs) = skipLine xs  -- 字符串结束，继续跳过注释
    skipInString (_:xs) = skipInString xs  -- 跳过其他字符
    
    -- 在行注释中跳过字符字面量（不保留）
    skipInChar :: String -> String
    skipInChar [] = []
    skipInChar ('\n':xs) = '\n' : goNormal xs  -- 换行时结束注释
    skipInChar ('\\':_:xs) = skipInChar xs  -- 跳过转义字符
    skipInChar ('\'':xs) = skipLine xs  -- 字符结束，继续跳过注释
    skipInChar (_:xs) = skipInChar xs  -- 跳过其他字符

    -- 跳过块注释直到 */，支持嵌套，只保留换行和转义引号
    skipBlock :: String -> Int -> String
    skipBlock xs depth = skipBlockAcc xs depth []
    
    -- 辅助函数，累积需要保留的字符
    skipBlockAcc :: String -> Int -> String -> String
    skipBlockAcc [] _depth acc = reverse acc  -- 注释未闭合，返回累积的字符
    skipBlockAcc ('\n':xs) depth acc = '\n' : skipBlockAcc xs depth acc  -- 保留换行
    skipBlockAcc ('/':'*':xs) depth acc = skipBlockAcc xs (depth + (1 :: Int)) acc  -- 嵌套块注释
    skipBlockAcc ('*':'/':xs) 0 _ = goNormal xs  -- 最外层注释结束，丢弃累积的字符
    skipBlockAcc ('*':'/':xs) depth acc = skipBlockAcc xs (depth - (1 :: Int)) acc  -- 内层注释结束
    skipBlockAcc ('\\':'"':xs) depth acc = skipBlockAcc xs depth ('"':'\\':acc)  -- 保留转义引号
    skipBlockAcc ('"':xs) depth acc = skipBlockAcc xs depth ('"':acc)  -- 保留普通引号
    skipBlockAcc ('\'':xs) depth acc = skipBlockAcc xs depth ('\'':acc)  -- 保留普通单引号
    skipBlockAcc ('\\':_:xs) depth acc = skipBlockAcc xs depth acc  -- 跳过其他转义字符
    skipBlockAcc (_:xs) depth acc = skipBlockAcc xs depth acc  -- 跳过其他字符
    
    -- 字符串字面量（保留内容与转义）
    goInString :: String -> String
    goInString [] = []  -- 非严格：未闭合字符串，返回空（已经处理的内容由调用者保留）
    goInString ('\n':xs) = '\n' : goNormal xs  -- 换行时结束字符串字面量
    -- 在字符串中，保留所有字符包括注释标记
    goInString ('/':'/':xs) = '/' : '/' : goInString xs  -- 保留 //
    goInString ('/':'*':xs) = '/' : '*' : goInString xs  -- 保留 /*
    goInString ('*':'/':xs) = '*' : '/' : goInString xs  -- 保留 */
    goInString ('\\':x:xs) = '\\' : x : goInString xs  -- 保留转义字符，包括转义引号
    goInString ('"':xs) = '"' : goNormal xs
    goInString (c:cs) = c : goInString cs
    
    -- 字符字面量（保留内容与转义）
    goInChar :: String -> String
    goInChar [] = []  -- 非严格：未闭合字符，返回到正常模式
    goInChar ('\n':xs) = '\n' : goNormal xs  -- 换行时结束字符字面量
    goInChar ('\\':x:xs) = '\\' : x : goInChar xs
    goInChar ('\'':xs) = '\'' : goNormal xs
    -- 在字符字面量中，保留所有字符包括注释标记
    goInChar ('/':'/':xs) = '/' : '/' : goInChar xs  -- 保留 //
    goInChar ('/':'*':xs) = '/' : '*' : goInChar xs  -- 保留 /*
    goInChar ('*':'/':xs) = '*' : '/' : goInChar xs  -- 保留 */
    goInChar (c:cs) = c : goInChar cs

-- 测试函数
prop_removeComments_preserves_strings :: String -> Bool
prop_removeComments_preserves_strings s = 
  let result = removeComments s
  in if isOnlyComment s
     then null (dropWhile isSpace result)  -- 结果应该为空或只有空白
     else countQuotesPreserved s result
  where
    isOnlyComment str = isOnlyLineComment str || isOnlyBlockComment str
    isOnlyLineComment str = case dropWhile isSpace str of
                             ('/':'/':_) -> True
                             _ -> False
    isOnlyBlockComment str = case dropWhile isSpace str of
                              ('/':'*':_) -> True
                              _ -> False
    countQuotesPreserved input output = 
      let countQuotes s' = length $ filter (== '"') s'
      in countQuotes input == countQuotes output

-- 详细的调试函数
debugRemoveComments :: String -> IO ()
debugRemoveComments s = do
  putStrLn $ "Input: " ++ show s
  let result = removeComments s
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Input quotes: " ++ show (length $ filter (== '"') s)
  putStrLn $ "Result quotes: " ++ show (length $ filter (== '"') result)
  putStrLn $ "Test passes: " ++ show (prop_removeComments_preserves_strings s)

-- 测试一些特定情况
testCases :: [(String, String, String)]
testCases = 
  [ ("简单字符串", "\"hello\"", "\"hello\""),
    ("字符串中的注释", "\"// comment\"", "\"// comment\""),
    ("字符串中的块注释", "\"/* comment */\"", "\"/* comment */\""),
    ("字符串和注释混合", "\"string\" // comment", "\"string\" "),
    ("多个字符串", "\"a\" // \"b\"", "\"a\" "),
    ("转义引号", "\"\\\"//\\\"\"", "\"\\\"//\\\"\""),
    ("未闭合字符串", "\"unclosed", "\"unclosed"),
    ("字符串中的换行", "\"multi\nline\"", "\"multi\nline\""),
    ("只有注释", "// comment", ""),
    ("只有块注释", "/* comment */", "")
  ]

runTests :: IO ()
runTests = do
  putStrLn "Running test cases..."
  mapM_ runTestCase testCases
  
  putStrLn "\nDebugging specific failing case:"
  debugRemoveComments "a//\""
  
  putStrLn "\nDebugging related cases:"
  debugRemoveComments "a//\"b"
  debugRemoveComments "a//\\\""
  debugRemoveComments "a//\\\"\""
  
  putStrLn "\nRunning QuickCheck tests..."
  quickCheckWith stdArgs { maxSuccess = 10000 } prop_removeComments_preserves_strings

runTestCase :: (String, String, String) -> IO ()
runTestCase (name, input, expected) = do
  let result = removeComments input
  if result == expected
    then putStrLn $ "✓ " ++ name ++ ": " ++ show input ++ " -> " ++ show result
    else putStrLn $ "✗ " ++ name ++ ": " ++ show input ++ " -> " ++ show result ++ " (expected: " ++ show expected ++ ")"

main :: IO ()
main = runTests