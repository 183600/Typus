import Data.List (isInfixOf)
import Data.Char (isSpace)

-- 直接从 Utils.hs 复制的 removeComments 函数
removeComments :: String -> String
removeComments s = 
  -- 直接使用通用的注释处理逻辑，这样可以正确处理字符串中的注释标记
  if all isSpace s
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
    goNormal ('"':xs) = '\"' : goInString xs
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
    skipBlockAcc ('\\':'\"':xs) depth acc = skipBlockAcc xs depth ('\"':'\\':acc)  -- 保留转义引号
    skipBlockAcc ('"':xs) depth acc = skipBlockAcc xs depth ('\"':acc)  -- 保留普通引号
    skipBlockAcc ('\'':xs) depth acc = skipBlockAcc xs depth ('\'':acc)  -- 保留普通单引号
    skipBlockAcc ('\\':_:xs) depth acc = skipBlockAcc xs depth acc  -- 跳过其他转义字符
    skipBlockAcc (_:xs) depth acc = skipBlockAcc xs depth acc  -- 跳过其他字符
    
    -- 字符串字面量（保留内容与转义）
    goInString :: String -> String
    goInString [] = []  -- 非严格：未闭合字符串，返回空（已经处理的内容由调用者保留）
    goInString ('\\':x:xs) = '\\' : x : goInString xs  -- 保留转义字符，包括转义引号（最具体的模式）
    goInString ('/':'/':xs) = '/' : '/' : goInString xs  -- 保留 //
    goInString ('/':'*':xs) = '/' : '*' : goInString xs  -- 保留 /*
    goInString ('*':'/':xs) = '*' : '/' : goInString xs  -- 保留 */
    goInString ('"':xs) = '\"' : goNormal xs  -- 结束字符串
    goInString ('\n':xs) = '\n' : goNormal xs  -- 换行时结束字符串字面量
    goInString (c:cs) = c : goInString cs  -- 其他字符
    
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

-- 测试失败用例
main :: IO ()
main = do
  let input = "a//\""
  putStrLn $ "Input: " ++ show input
  let result = removeComments input
  putStrLn $ "Result: " ++ show result
  
  -- 手动追踪 goNormal 的执行
  putStrLn "\nTracing goNormal execution:"
  let traceGoNormal [] = "[]"
      traceGoNormal ('"':xs) = "\"" ++ traceGoInString xs
      traceGoNormal ('\'':xs) = "'" ++ traceGoInChar xs
      traceGoNormal ('/':'/':xs) = "skipLine(" ++ show xs ++ ")"
      traceGoNormal ('/':'*':xs) = "skipBlock(" ++ show xs ++ ", 0)"
      traceGoNormal ('/':xs) = "/" ++ traceGoNormal xs
      traceGoNormal (c:cs) = [c] ++ traceGoNormal cs
      
      traceGoInString [] = "[]"
      traceGoInString ('\\':x:xs) = "\\" ++ [x] ++ traceGoInString xs
      traceGoInString ('/':'/':xs) = "//" ++ traceGoInString xs
      traceGoInString ('/':'*':xs) = "/*" ++ traceGoInString xs
      traceGoInString ('*':'/':xs) = "*/" ++ traceGoInString xs
      traceGoInString ('"':xs) = "\"" ++ traceGoNormal xs
      traceGoInString ('\n':xs) = "\\n" ++ traceGoNormal xs
      traceGoInString (c:cs) = [c] ++ traceGoInString cs
      
      traceGoInChar [] = "[]"
      traceGoInChar ('\\':x:xs) = "\\" ++ [x] ++ traceGoInChar xs
      traceGoInChar ('\'':xs) = "'" ++ traceGoNormal xs
      traceGoInChar ('/':'/':xs) = "//" ++ traceGoInChar xs
      traceGoInChar ('/':'*':xs) = "/*" ++ traceGoInChar xs
      traceGoInChar ('*':'/':xs) = "*/" ++ traceGoInChar xs
      traceGoInChar (c:cs) = [c] ++ traceGoInChar cs
  
  putStrLn $ "Trace: " ++ traceGoNormal input