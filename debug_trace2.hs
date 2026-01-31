-- 导入 trace 函数
import Debug.Trace (trace)

-- 带调试信息的 goInString 函数
goInString :: String -> String
goInString xs = trace ("goInString: " ++ show xs) (goInString' xs)
  where
    goInString' [] = []
    goInString' ('\\':x:xs) = '\\' : x : goInString' xs
    goInString' ('/':'/':xs) = '/' : '/' : goInString' xs
    goInString' ('/':'*':xs) = '/' : '*' : goInString' xs
    goInString' ('*':'/':xs) = '*' : '/' : goInString' xs
    goInString' ('"':xs) = '"' : []
    goInString' ('\n':xs) = '\n' : []
    goInString' (c:cs) = c : goInString' cs

-- 带调试信息的 goNormal 函数
goNormal :: String -> String
goNormal xs = trace ("goNormal: " ++ show xs) (goNormal' xs)
  where
    goNormal' [] = []
    goNormal' ('"':xs) = '"' : goInString xs
    goNormal' ('\'':xs) = '\'' : []
    goNormal' ('/':'/':xs) = []
    goNormal' ('/':'*':xs) = []
    goNormal' ('/':xs) = '/' : goNormal' xs
    goNormal' (c:cs) = c : goNormal' cs

-- 测试函数
main :: IO ()
main = do
  let input = "\"a//\\\""
  putStrLn $ "Input: " ++ show input
  putStrLn $ "First char: " ++ show (head input)
  let result = goNormal input
  putStrLn $ "Result: " ++ show result