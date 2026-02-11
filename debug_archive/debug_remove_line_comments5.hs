import Utils hiding (isProblematicUnclosedString, isCompleteStringLiteral)
import Data.List

main :: IO ()
main = do
    let line = "\"// not comment\""
    putStrLn $ "Processing line: " ++ show line
    putStrLn $ "Is problematic: " ++ show (isProblematicUnclosedString ('"':take 10 (tail line)))
    let result = processLine line
    putStrLn $ "Result: " ++ show result
    
    putStrLn "\nTesting goInString directly:"
    let stringContent = "// not comment\""
    putStrLn $ "Input to goInString: " ++ show stringContent
    let stringResult = goInString stringContent
    putStrLn $ "goInString result: " ++ show stringResult

processLine :: String -> String
processLine [] = []
processLine ('"':xs) = 
  if isProblematicUnclosedString ('"':take 10 xs)
    then '"' : goProblematicString xs
    else '"' : goInString xs
processLine ('\'':xs) = '\'' : goInChar xs
processLine ('/':'/':_) = []  -- 遇到行注释，停止处理
processLine (c:cs) = c : processLine cs

goProblematicString :: String -> String
goProblematicString [] = []
goProblematicString ('\n':cs) = '\n' : processLine cs  -- 换行后返回处理下一行
goProblematicString ('/':'/':cs) = []  -- 遇到行注释，停止处理
goProblematicString (c:cs) = c : goProblematicString cs

goInString :: String -> String
goInString [] = ""  -- 未闭合字符串，不添加引号
goInString ('\\':[]) = "\\"  -- 反斜杠在末尾，不添加引号
goInString ('\\':x:xs) = '\\' : x : goInString xs  -- 保留转义字符，包括转义引号
goInString ('"':xs) = '"' : goAfterString xs  -- 结束字符串，检查后面是否有注释
goInString (c:cs) = c : goInString cs  -- 其他字符

goAfterString :: String -> String
goAfterString [] = []
goAfterString ('/':'/':_) = []  -- 字符串后遇到注释，停止处理
goAfterString (c:cs) = c : goAfterString cs  -- 其他字符继续处理

goInChar :: String -> String
goInChar [] = []  -- 未闭合字符，不添加引号
goInChar ('\\':x:xs) = '\\' : x : goInChar xs  -- 保留转义字符
goInChar ('\'':xs) = '\'' : processLine xs  -- 结束字符字面量，返回正常处理
goInChar (c:cs) = c : goInChar cs  -- 其他字符

isProblematicUnclosedString :: String -> Bool
isProblematicUnclosedString s = 
  if null s 
    then False
    else case s of
      "\\" -> True
      "'" -> True
      "\"\\" -> True
      "\"\\\\" -> True
      "'\\" -> True
      (c:_) -> c `elem` ['"', '\''] && not (isCompleteStringLiteral s)

isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    ['\''] -> False
    ['"'] -> False
    "\"\\" -> True
    "\\" -> False
    "\"" -> False
    "'" -> False
    (c:rest) -> case c of
           '"' -> hasClosingQuote '"' rest
           '\'' -> False  -- 单引号字符串总是返回False

hasClosingQuote :: Char -> String -> Bool
hasClosingQuote quote [] = False
hasClosingQuote quote (x:xs) = 
  if x == quote then True  -- 找到闭合引号，是完整的字符串
  else hasClosingQuote quote xs  -- 其他字符，继续查找