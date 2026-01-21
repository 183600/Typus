-- 模拟removeComments处理过程
import Data.List (isPrefixOf, isSuffixOf)

-- 模拟goNormal函数的关键部分
goNormal :: String -> String
goNormal [] = []
goNormal ('"':xs) = '"' : goInString xs
goNormal ('\'':xs) = '\'' : goInChar xs
goNormal ('/':'/':'*':xs) = '/' : skipBlock xs 0
goNormal ('/':'/':xs) = skipLine xs
goNormal ('/':'*':xs) = skipBlock xs 0
goNormal ('/':xs) = '/' : goNormal xs
goNormal (c:cs) = c : goNormal cs

skipLine :: String -> String
skipLine [] = []
skipLine ('\n':xs) = '\n' : goNormal xs
skipLine ('"':xs) = '"' : skipLine xs
skipLine (_:xs) = skipLine xs

skipBlock :: String -> Int -> String
skipBlock [] _depth = []
skipBlock ('\n':xs) _depth = '\n' : skipBlock xs _depth
skipBlock ('/':'*':xs) depth = skipBlock xs (depth + 1)
skipBlock ('*':'/':xs) 0 = goNormal xs
skipBlock ('*':'/':xs) depth = skipBlock xs (depth - 1)
skipBlock ('\\':x:xs) depth = '\\' : x : skipBlock xs depth
skipBlock ('"':xs) depth = '"' : skipBlock xs depth
skipBlock (_:xs) _depth = skipBlock xs _depth

goInString :: String -> String
goInString [] = []
goInString ('\n':xs) = '\n' : goNormal xs
goInString ('/':'/':xs) = '/' : '/' : goInString xs
goInString ('/':'*':xs) = '/' : '*' : goInString xs
goInString ('*':'/':xs) = '*' : '/' : goInString xs
goInString ('\\':x:xs) = '\\' : x : goInString xs
goInString ('"':xs) = '"' : goNormal xs
goInString (c:cs) = c : goInString cs

goInChar :: String -> String
goInChar [] = []
goInChar ('\n':xs) = '\n' : goNormal xs
goInChar ('\\':x:xs) = '\\' : x : goInChar xs
goInChar ('\'':xs) = '\'' : goNormal xs
goInChar ('/':'/':xs) = '/' : '/' : goInChar xs
goInChar ('/':'*':xs) = '/' : '*' : goInChar xs
goInChar ('*':'/':xs) = '*' : '/' : goInChar xs
goInChar (c:cs) = c : goInChar cs

main :: IO ()
main = do
  let input = "code /* comment */ more code"
  let result = goNormal input
  putStrLn $ "Input: " ++ show input
  putStrLn $ "Result: " ++ show result
  putStrLn $ "Starts with 'code': " ++ show ("code" `isPrefixOf` result)
  putStrLn $ "Ends with 'more code ': " ++ show ("more code " `isSuffixOf` result)
  
  -- 手动分析
  putStrLn "\n--- Manual Analysis ---"
  let code = take 5 input  -- "code"
  let space1 = input !! 5  -- ' '
  let commentStart = take 2 (drop 6 input)  -- "/*"
  let commentEnd = take 2 (drop 17 input)  -- "*/"
  let space2 = input !! 19  -- ' '
  let moreCode = drop 20 input  -- "more code"
  
  putStrLn $ "code: " ++ show code
  putStrLn $ "space1: " ++ show space1
  putStrLn $ "commentStart: " ++ show commentStart
  putStrLn $ "commentEnd: " ++ show commentEnd
  putStrLn $ "space2: " ++ show space2
  putStrLn $ "moreCode: " ++ show moreCode