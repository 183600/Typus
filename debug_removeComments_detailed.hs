import Data.List (isInfixOf)
import Data.Char (isSpace)

-- 带调试信息的 removeComments 函数
removeCommentsDebug :: String -> IO String
removeCommentsDebug s = do
  putStrLn $ "removeComments called with: " ++ show s
  let result = goNormal s
  putStrLn $ "removeComments result: " ++ show result
  return result
  where
    -- 如果字符串不包含注释，直接返回原字符串
    goNormal :: String -> String
    goNormal xs = goNormal' xs 0
      where
        goNormal' [] depth = []
        goNormal' ('"':xs) depth = 
          putStrLn ("Entering goInString at depth " ++ show depth ++ " with: " ++ show ('"':xs)) >>
          let result = '\"' : goInString xs depth
          in putStrLn ("goInString returned: " ++ show result) >> return result
        goNormal' ('\'':xs) depth = 
          putStrLn ("Entering goInChar at depth " ++ show depth ++ " with: " ++ show ('\'':xs)) >>
          let result = '\'' : goInChar xs depth
          in putStrLn ("goInChar returned: " ++ show result) >> return result
        goNormal' ('/':'/':xs) depth = 
          putStrLn ("Entering skipLine at depth " ++ show depth ++ " with: " ++ show xs) >>
          let result = skipLine xs depth
          in putStrLn ("skipLine returned: " ++ show result) >> return result
        goNormal' ('/':'*':xs) depth = 
          putStrLn ("Entering skipBlock at depth " ++ show depth ++ " with: " ++ show xs) >>
          let result = skipBlock xs depth
          in putStrLn ("skipBlock returned: " ++ show result) >> return result
        goNormal' ('/':xs) depth = 
          putStrLn ("Single / at depth " ++ show depth) >>
          let result = '/' : (unsafePerformIO $ goNormal' xs depth)
          in return result
        goNormal' (c:cs) depth = 
          putStrLn ("Processing char '" ++ [c] ++ "' at depth " ++ show depth) >>
          let result = c : (unsafePerformIO $ goNormal' cs depth)
          in return result

    -- 字符串字面量（保留内容与转义）
    goInString :: String -> Int -> String
    goInString xs depth = goInString' xs depth
      where
        goInString' [] _ = []
        goInString' ('\n':xs) depth = 
          putStrLn ("Newline in string at depth " ++ show depth) >>
          let result = '\n' : (unsafePerformIO $ goNormal' xs depth)
          in return result
        goInString' ('/':'/':xs) depth = 
          putStrLn ("Found // in string at depth " ++ show depth) >>
          let result = '/' : '/' : (unsafePerformIO $ goInString' xs depth)
          in return result
        goInString' ('/':'*':xs) depth = 
          putStrLn ("Found /* in string at depth " ++ show depth) >>
          let result = '/' : '*' : (unsafePerformIO $ goInString' xs depth)
          in return result
        goInString' ('*':'/':xs) depth = 
          putStrLn ("Found */ in string at depth " ++ show depth) >>
          let result = '*' : '/' : (unsafePerformIO $ goInString' xs depth)
          in return result
        goInString' ('\\':x:xs) depth = 
          putStrLn ("Found escape sequence \\" ++ [x] ++ " in string at depth " ++ show depth) >>
          let result = '\\' : x : (unsafePerformIO $ goInString' xs depth)
          in return result
        goInString' ('"':xs) depth = 
          putStrLn ("Found closing quote in string at depth " ++ show depth) >>
          let result = '\"' : (unsafePerformIO $ goNormal' xs depth)
          in return result
        goInString' (c:cs) depth = 
          putStrLn ("Processing char '" ++ [c] ++ "' in string at depth " ++ show depth) >>
          let result = c : (unsafePerformIO $ goInString' cs depth)
          in return result

    -- 字符字面量（保留内容与转义）
    goInChar :: String -> Int -> String
    goInChar xs depth = goInChar' xs depth
      where
        goInChar' [] _ = []
        goInChar' ('\n':xs) depth = 
          putStrLn ("Newline in char at depth " ++ show depth) >>
          let result = '\n' : (unsafePerformIO $ goNormal' xs depth)
          in return result
        goInChar' ('\\':x:xs) depth = 
          putStrLn ("Found escape sequence \\\" ++ [x] ++ " in char at depth " ++ show depth) >>
          let result = '\\' : x : (unsafePerformIO $ goInChar' xs depth)
          in return result
        goInChar' ('\'':xs) depth = 
          putStrLn ("Found closing quote in char at depth " ++ show depth) >>
          let result = '\'' : (unsafePerformIO $ goNormal' xs depth)
          in return result
        goInChar' (c:cs) depth = 
          putStrLn ("Processing char '" ++ [c] ++ "' in char at depth " ++ show depth) >>
          let result = c : (unsafePerformIO $ goInChar' cs depth)
          in return result

    -- 跳过行注释直到换行，只保留换行
    skipLine :: String -> Int -> String
    skipLine xs depth = skipLine' xs depth
      where
        skipLine' [] _ = []
        skipLine' ('\n':xs) depth = 
          putStrLn ("Newline ending line comment at depth " ++ show depth) >>
          let result = '\n' : (unsafePerformIO $ goNormal' xs depth)
          in return result
        skipLine' (c:cs) depth = 
          putStrLn ("Skipping char '" ++ [c] ++ "' in line comment at depth " ++ show depth) >>
          let result = unsafePerformIO $ skipLine' cs depth
          in return result

    -- 跳过块注释直到 */，支持嵌套，只保留换行和转义引号
    skipBlock :: String -> Int -> String
    skipBlock xs depth = skipBlock' xs depth []
      where
        skipBlock' [] _ acc = reverse acc
        skipBlock' ('\n':xs) depth acc = 
          putStrLn ("Newline in block comment at depth " ++ show depth) >>
          let result = '\n' : (unsafePerformIO $ skipBlock' xs depth acc)
          in return result
        skipBlock' ('/':'*':xs) depth acc = 
          putStrLn ("Found nested block comment at depth " ++ show depth) >>
          let result = unsafePerformIO $ skipBlock' xs (depth + 1) acc
          in return result
        skipBlock' ('*':'/':xs) 0 _ = 
          putStrLn ("Block comment ended at depth 0") >>
          let result = unsafePerformIO $ goNormal' xs 0
          in return result
        skipBlock' ('*':'/':xs) depth acc = 
          putStrLn ("Inner block comment ended at depth " ++ show depth) >>
          let result = unsafePerformIO $ skipBlock' xs (depth - 1) acc
          in return result
        skipBlock' (c:cs) depth acc = 
          putStrLn ("Skipping char '" ++ [c] ++ "' in block comment at depth " ++ show depth) >>
          let result = unsafePerformIO $ skipBlock' cs depth acc
          in return result

-- 使用 unsafePerformIO 来简化调试
import System.IO.Unsafe (unsafePerformIO)

-- 测试函数
main :: IO ()
main = do
  putStrLn "Testing removeComments with debug info:"
  result <- removeCommentsDebug "a//\""
  putStrLn $ "Final result: " ++ show result