import Data.List (isInfixOf)
import Data.Char (isSpace)

-- 带调试信息的 removeComments 函数
removeComments :: String -> String
removeComments s = 
  putStrLn ("removeComments called with: " ++ show s) `seq`
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
    goNormal ('"':xs) = 
      putStrLn ("goNormal: found opening quote, entering string mode with: " ++ show xs) `seq`
      '"' : goInString xs
    goNormal ('\'':xs) = 
      putStrLn ("goNormal: found opening single quote, entering char mode with: " ++ show xs) `seq`
      '\'' : goInChar xs
    goNormal ('/':'/':xs) = 
      putStrLn ("goNormal: found //, entering line comment mode with: " ++ show xs) `seq`
      skipLine xs
    goNormal ('/':'*':xs) = 
      putStrLn ("goNormal: found /*, entering block comment mode with: " ++ show xs) `seq`
      skipBlock xs 0
    goNormal ('/':xs) = 
      putStrLn ("goNormal: found single / with: " ++ show xs) `seq`
      '/' : goNormal xs
    goNormal (c:cs) = 
      putStrLn ("goNormal: processing character '" ++ [c] ++ "' with: " ++ show cs) `seq`
      c : goNormal cs

    -- 跳过行注释直到换行，只保留换行
    skipLine :: String -> String
    skipLine [] = []
    skipLine ('\n':xs) = 
      putStrLn ("skipLine: found newline, returning to normal mode with: " ++ show xs) `seq`
      '\n' : goNormal xs
    skipLine ('"':xs) = 
      putStrLn ("skipLine: found string literal in comment, skipping with: " ++ show xs) `seq`
      skipInString xs
    skipLine ('\'':xs) = 
      putStrLn ("skipLine: found char literal in comment, skipping with: " ++ show xs) `seq`
      skipInChar xs
    skipLine (c:cs) = 
      putStrLn ("skipLine: skipping character '" ++ [c] ++ "' with: " ++ show cs) `seq`
      skipLine cs
    
    -- 在行注释中跳过字符串字面量（不保留）
    skipInString :: String -> String
    skipInString [] = []
    skipInString ('\n':xs) = 
      putStrLn ("skipInString: found newline, returning to normal mode with: " ++ show xs) `seq`
      '\n' : goNormal xs
    skipInString ('\\':_:xs) = 
      putStrLn ("skipInString: found escape, skipping with: " ++ show xs) `seq`
      skipInString xs
    skipInString ('"':xs) = 
      putStrLn ("skipInString: found closing quote, returning to skipLine with: " ++ show xs) `seq`
      skipLine xs
    skipInString (c:cs) = 
      putStrLn ("skipInString: skipping character '" ++ [c] ++ "' with: " ++ show cs) `seq`
      skipInString cs
    
    -- 在行注释中跳过字符字面量（不保留）
    skipInChar :: String -> String
    skipInChar [] = []
    skipInChar ('\n':xs) = 
      putStrLn ("skipInChar: found newline, returning to normal mode with: " ++ show xs) `seq`
      '\n' : goNormal xs
    skipInChar ('\\':_:xs) = 
      putStrLn ("skipInChar: found escape, skipping with: " ++ show xs) `seq`
      skipInChar xs
    skipInChar ('\'':xs) = 
      putStrLn ("skipInChar: found closing quote, returning to skipLine with: " ++ show xs) `seq`
      skipLine xs
    skipInChar (c:cs) = 
      putStrLn ("skipInChar: skipping character '" ++ [c] ++ "' with: " ++ show cs) `seq`
      skipInChar cs

    -- 跳过块注释直到 */，支持嵌套，只保留换行和转义引号
    skipBlock :: String -> Int -> String
    skipBlock xs depth = 
      putStrLn ("skipBlock: starting with depth " ++ show depth ++ " and: " ++ show xs) `seq`
      skipBlockAcc xs depth []
    
    -- 辅助函数，累积需要保留的字符
    skipBlockAcc :: String -> Int -> String -> String
    skipBlockAcc [] _depth acc = 
      putStrLn ("skipBlockAcc: end of input, returning accumulated: " ++ show (reverse acc)) `seq`
      reverse acc
    skipBlockAcc ('\n':xs) depth acc = 
      putStrLn ("skipBlockAcc: found newline at depth " ++ show depth ++ ", preserving") `seq`
      '\n' : skipBlockAcc xs depth acc
    skipBlockAcc ('/':'*':xs) depth acc = 
      putStrLn ("skipBlockAcc: found nested block comment at depth " ++ show depth) `seq`
      skipBlockAcc xs (depth + 1) acc
    skipBlockAcc ('*':'/':xs) 0 _ = 
      putStrLn ("skipBlockAcc: block comment ended at depth 0") `seq`
      goNormal xs
    skipBlockAcc ('*':'/':xs) depth acc = 
      putStrLn ("skipBlockAcc: inner block comment ended at depth " ++ show depth) `seq`
      skipBlockAcc xs (depth - 1) acc
    skipBlockAcc ('\\':'"':xs) depth acc = 
      putStrLn ("skipBlockAcc: found escaped quote at depth " ++ show depth) `seq`
      skipBlockAcc xs depth ('"':'\\':acc)
    skipBlockAcc ('"':xs) depth acc = 
      putStrLn ("skipBlockAcc: found quote at depth " ++ show depth) `seq`
      skipBlockAcc xs depth ('"':acc)
    skipBlockAcc ('\'':xs) depth acc = 
      putStrLn ("skipBlockAcc: found single quote at depth " ++ show depth) `seq`
      skipBlockAcc xs depth ('\'':acc)
    skipBlockAcc ('\\':_:xs) depth acc = 
      putStrLn ("skipBlockAcc: found other escape at depth " ++ show depth) `seq`
      skipBlockAcc xs depth acc
    skipBlockAcc (c:cs) depth acc = 
      putStrLn ("skipBlockAcc: skipping character '" ++ [c] ++ "' at depth " ++ show depth) `seq`
      skipBlockAcc cs depth acc
    
    -- 字符串字面量（保留内容与转义）
    goInString :: String -> String
    goInString [] = 
      putStrLn "goInString: end of input" `seq`
      []
    goInString ('\n':xs) = 
      putStrLn ("goInString: found newline, returning to normal mode with: " ++ show xs) `seq`
      '\n' : goNormal xs
    goInString ('/':'/':xs) = 
      putStrLn ("goInString: found // in string, preserving with: " ++ show xs) `seq`
      '/' : '/' : goInString xs
    goInString ('/':'*':xs) = 
      putStrLn ("goInString: found /* in string, preserving with: " ++ show xs) `seq`
      '/' : '*' : goInString xs
    goInString ('*':'/':xs) = 
      putStrLn ("goInString: found */ in string, preserving with: " ++ show xs) `seq`
      '*' : '/' : goInString xs
    goInString ('\\':x:xs) = 
      putStrLn ("goInString: found escape sequence \\" ++ [x] ++ " in string, preserving with: " ++ show xs) `seq`
      '\\' : x : goInString xs
    goInString ('"':xs) = 
      putStrLn ("goInString: found closing quote, returning to normal mode with: " ++ show xs) `seq`
      '"' : goNormal xs
    goInString (c:cs) = 
      putStrLn ("goInString: processing character '" ++ [c] ++ "' in string with: " ++ show cs) `seq`
      c : goInString cs
    
    -- 字符字面量（保留内容与转义）
    goInChar :: String -> String
    goInChar [] = 
      putStrLn "goInChar: end of input" `seq`
      []
    goInChar ('\n':xs) = 
      putStrLn ("goInChar: found newline, returning to normal mode with: " ++ show xs) `seq`
      '\n' : goNormal xs
    goInChar ('\\':x:xs) = 
      putStrLn ("goInChar: found escape sequence \\" ++ [x] ++ " in char, preserving with: " ++ show xs) `seq`
      '\\' : x : goInChar xs
    goInChar ('\'':xs) = 
      putStrLn ("goInChar: found closing quote, returning to normal mode with: " ++ show xs) `seq`
      '\'' : goNormal xs
    goInChar ('/':'/':xs) = 
      putStrLn ("goInChar: found // in char, preserving with: " ++ show xs) `seq`
      '/' : '/' : goInChar xs
    goInChar ('/':'*':xs) = 
      putStrLn ("goInChar: found /* in char, preserving with: " ++ show xs) `seq`
      '/' : '*' : goInChar xs
    goInChar ('*':'/':xs) = 
      putStrLn ("goInChar: found */ in char, preserving with: " ++ show xs) `seq`
      '*' : '/' : goInChar xs
    goInChar (c:cs) = 
      putStrLn ("goInChar: processing character '" ++ [c] ++ "' in char with: " ++ show cs) `seq`
      c : goInChar cs

-- 测试函数
main :: IO ()
main = do
  putStrLn $ "Testing with input: \"a//\\\"\""
  let result = removeComments "a//\""
  putStrLn $ "Final result: " ++ show result