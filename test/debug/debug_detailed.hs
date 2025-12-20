import Data.List (isInfixOf)
import System.IO.Unsafe (unsafePerformIO)

-- 带调试的 skipBlock 函数
removeCommentsDebug :: String -> String
removeCommentsDebug = goNormal
  where
    goNormal [] = []
    goNormal ('/':'*':xs) = skipBlock xs "/*"
    goNormal (c:cs) = c : goNormal cs

    skipBlock xs commentStart = go 1 xs commentStart
      where
        go n remaining prefix = 
          trace ("go n=" ++ show n ++ " remaining=" ++ take 10 (show remaining) ++ " prefix=" ++ take 20 prefix) $
          case (n, remaining) of
            (1, '*':'/':xs) -> 
              trace ("Outer comment ended, switching to goNormal with: " ++ take 10 xs) $
              goNormal xs
            (n, '*':'/':xs) -> 
              trace ("Inner comment ended, reducing nesting from " ++ show n ++ " to " ++ show (n-1)) $
              go (n-1) xs (prefix ++ "*/")
            (n, '/':'*':xs) -> 
              trace ("New nested comment found, increasing nesting from " ++ show n ++ " to " ++ show (n+1)) $
              go (n+1) xs (prefix ++ "/*")
            (n, '\n':xs) -> 
              '\n' : go n xs (prefix ++ "\n")
            (n, c:xs) -> 
              go n xs (prefix ++ [c])
            (1, []) -> 
              trace ("Outer comment not closed") $
              []
            (n, []) -> 
              trace ("Inner comment not closed") $
              []

-- 简单的 trace 实现
trace :: String -> a -> a
trace s x = 
  unsafePerformIO $
    putStrLn s >>
    return x

-- 测试函数
testProblem :: IO ()
testProblem = do
    putStrLn "=== Testing problematic case ==="
    let input = "/* /* */text"
    let result = removeCommentsDebug input
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Expected: \"text\""
    
    putStrLn "\n=== Testing correct case ==="
    let input2 = "/* comment */text"
    let result2 = removeCommentsDebug input2
    putStrLn $ "Input: " ++ show input2
    putStrLn $ "Result: " ++ show result2
    
    putStrLn "\n=== Testing original failing case ==="
    let input3 = "/* outer /* inner */a"
    let result3 = removeCommentsDebug input3
    putStrLn $ "Input: " ++ show input3
    putStrLn $ "Result: " ++ show result3

main :: IO ()
main = testProblem