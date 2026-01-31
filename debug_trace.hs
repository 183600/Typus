import Data.List (isInfixOf)
import Data.Char (isSpace)

-- 带调试信息的 goInString 函数
goInStringDebug :: String -> String
goInStringDebug [] = do
  putStrLn "goInString: end of input"
  []
goInStringDebug ('\n':xs) = do
  putStrLn "goInString: found newline, returning to normal mode"
  '\n' : goNormalDebug xs
goInStringDebug ('/':'/':xs) = do
  putStrLn "goInString: found // in string, preserving"
  '/' : '/' : goInStringDebug xs
goInStringDebug ('/':'*':xs) = do
  putStrLn "goInString: found /* in string, preserving"
  '/' : '*' : goInStringDebug xs
goInStringDebug ('*':'/':xs) = do
  putStrLn "goInString: found */ in string, preserving"
  '*' : '/' : goInStringDebug xs
goInStringDebug ('\\':x:xs) = do
  putStrLn $ "goInString: found escape sequence \\" ++ [x] ++ " in string, preserving"
  '\\' : x : goInStringDebug xs
goInStringDebug ('"':xs) = do
  putStrLn "goInString: found closing quote, returning to normal mode"
  '"' : goNormalDebug xs
goInStringDebug (c:cs) = do
  putStrLn $ "goInString: processing character '" ++ [c] ++ "' in string"
  c : goInStringDebug cs

-- 带调试信息的 goNormal 函数
goNormalDebug :: String -> String
goNormalDebug [] = do
  putStrLn "goNormal: end of input"
  []
goNormalDebug ('"':xs) = do
  putStrLn "goNormal: found opening quote, entering string mode"
  '"' : goInStringDebug xs
goNormalDebug ('\'':xs) = do
  putStrLn "goNormal: found opening single quote, entering char mode"
  '\'' : goInCharDebug xs
goNormalDebug ('/':'/':xs) = do
  putStrLn "goNormal: found //, entering line comment mode"
  skipLineDebug xs
goNormalDebug ('/':'*':xs) = do
  putStrLn "goNormal: found /*, entering block comment mode"
  skipBlockDebug xs 0
goNormalDebug ('/':xs) = do
  putStrLn "goNormal: found single /, preserving"
  '/' : goNormalDebug xs
goNormalDebug (c:cs) = do
  putStrLn $ "goNormal: processing character '" ++ [c] ++ "'"
  c : goNormalDebug cs

-- 带调试信息的 goInChar 函数
goInCharDebug :: String -> String
goInCharDebug [] = do
  putStrLn "goInChar: end of input"
  []
goInCharDebug ('\n':xs) = do
  putStrLn "goInChar: found newline, returning to normal mode"
  '\n' : goNormalDebug xs
goInCharDebug ('\\':x:xs) = do
  putStrLn $ "goInChar: found escape sequence \\" ++ [x] ++ " in char, preserving"
  '\\' : x : goInCharDebug xs
goInCharDebug ('\'':xs) = do
  putStrLn "goInChar: found closing quote, returning to normal mode"
  '\'' : goNormalDebug xs
goInCharDebug ('/':'/':xs) = do
  putStrLn "goInChar: found // in char, preserving"
  '/' : '/' : goInCharDebug xs
goInCharDebug ('/':'*':xs) = do
  putStrLn "goInChar: found /* in char, preserving"
  '/' : '*' : goInCharDebug xs
goInCharDebug ('*':'/':xs) = do
  putStrLn "goInChar: found */ in char, preserving"
  '*' : '/' : goInCharDebug xs
goInCharDebug (c:cs) = do
  putStrLn $ "goInChar: processing character '" ++ [c] ++ "' in char"
  c : goInCharDebug cs

-- 带调试信息的 skipLine 函数
skipLineDebug :: String -> String
skipLineDebug [] = do
  putStrLn "skipLine: end of input"
  []
skipLineDebug ('\n':xs) = do
  putStrLn "skipLine: found newline, returning to normal mode"
  '\n' : goNormalDebug xs
skipLineDebug (c:cs) = do
  putStrLn $ "skipLine: skipping character '" ++ [c] ++ "'"
  skipLineDebug cs

-- 带调试信息的 skipBlock 函数
skipBlockDebug :: String -> Int -> String
skipBlockDebug xs depth = skipBlockAccDebug xs depth []
  where
    skipBlockAccDebug [] _ acc = do
      putStrLn $ "skipBlock: end of input, returning accumulated: " ++ show (reverse acc)
      reverse acc
    skipBlockAccDebug ('\n':xs) depth acc = do
      putStrLn $ "skipBlock: found newline at depth " ++ show depth ++ ", preserving"
      '\n' : skipBlockAccDebug xs depth acc
    skipBlockAccDebug ('/':'*':xs) depth acc = do
      putStrLn $ "skipBlock: found nested block comment at depth " ++ show depth
      skipBlockAccDebug xs (depth + 1) acc
    skipBlockAccDebug ('*':'/':xs) 0 _ = do
      putStrLn "skipBlock: block comment ended at depth 0"
      goNormalDebug xs
    skipBlockAccDebug ('*':'/':xs) depth acc = do
      putStrLn $ "skipBlock: inner block comment ended at depth " ++ show depth
      skipBlockAccDebug xs (depth - 1) acc
    skipBlockAccDebug (c:cs) depth acc = do
      putStrLn $ "skipBlock: skipping character '" ++ [c] ++ "' at depth " ++ show depth
      skipBlockAccDebug cs depth acc

-- 测试函数
main :: IO ()
main = do
  putStrLn $ "Testing with input: \"a//\\\"\""
  let result = goNormalDebug "a//\""
  putStrLn $ "Final result: " ++ show result