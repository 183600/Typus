import Data.List (isInfixOf)

-- 带详细调试的 removeComments
removeCommentsDebug :: String -> String
removeCommentsDebug = goNormal
  where
    goNormal [] = []
    goNormal ('/':'*':xs) = 
      putStrLn ("Found /* at start, entering skipBlock") >>
      skipBlock xs 1
    goNormal (c:cs) = 
      putStrLn ("Normal processing: " ++ [c]) >>
      c : goNormal cs

    skipBlock [] n = 
      putStrLn ("SkipBlock: reached end with nesting " ++ show n) >>
      []
    skipBlock xs n = go n xs
      where
        go 0 remaining = 
          putStrLn ("SkipBlock: nesting reached 0, returning to goNormal") >>
          goNormal remaining
        go 1 ('*':'/':xs) = 
          putStrLn ("SkipBlock: outer comment ended, returning to goNormal") >>
          goNormal xs
        go n ('*':'/':xs) = 
          putStrLn ("SkipBlock: inner comment ended, nesting from " ++ show n ++ " to " ++ show (n-1)) >>
          go (n-1) xs
        go n ('/':'*':xs) = 
          putStrLn ("SkipBlock: new nested comment, nesting from " ++ show n ++ " to " ++ show (n+1)) >>
          go (n+1) xs
        go n ('\n':xs) = 
          putStrLn ("SkipBlock: preserving newline") >>
          '\n' : go n xs
        go n (c:xs) = 
          putStrLn ("SkipBlock: skipping char " ++ [c] ++ " with nesting " ++ show n) >>
          go n xs
        go n [] = 
          putStrLn ("SkipBlock: reached end with nesting " ++ show n) >>
          []

-- 测试函数
testProblemCase :: IO ()
testProblemCase = do
    putStrLn "=== Testing problematic case: \"/* /* */text\" ==="
    let input = "/* /* */text"
    putStrLn $ "Input: " ++ show input
    putStrLn "Processing:"
    let result = removeCommentsDebug input
    putStrLn $ "Final result: " ++ show result

main :: IO ()
main = testProblemCase