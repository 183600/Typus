import Utils (removeComments)

-- 带调试信息的removeComments
removeCommentsTrace :: String -> String
removeCommentsTrace = goNormal 0
  where
    goNormal depth [] = trace (replicate depth ' ' ++ "goNormal: []") []
    goNormal depth ('/':'/':xs) = trace (replicate depth ' ' ++ "goNormal: //") $ skipLine (depth + 2) xs
    goNormal depth ('/':'*':xs) = trace (replicate depth ' ' ++ "goNormal: /*") $ skipBlock (depth + 2) xs
    goNormal depth ('"':xs) = trace (replicate depth ' ' ++ "goNormal: \"" ) $ '"' : goInString (depth + 2) xs
    goNormal depth ('\'':xs) = trace (replicate depth ' ' ++ "goNormal: '") $ '\'' : goInChar (depth + 2) xs
    goNormal depth (c:cs) = trace (replicate depth ' ' ++ "goNormal: " ++ show c) $ c : goNormal depth cs

    skipLine depth [] = trace (replicate depth ' ' ++ "skipLine: []") []
    skipLine depth ('\n':xs) = trace (replicate depth ' ' ++ "skipLine: \\n") $ '\n' : goNormal (depth - 2) xs
    skipLine depth (_:xs) = skipLine depth xs

    skipBlock depth [] = trace (replicate depth ' ' ++ "skipBlock: []") []
    skipBlock depth xs = skipBlockAtLevel depth 1 xs
    
    skipBlockAtLevel depth 0 xs = trace (replicate depth ' ' ++ "skipBlockAtLevel 0: calling goNormal") $ goNormal (depth - 2) xs
    skipBlockAtLevel depth n [] = trace (replicate depth ' ' ++ "skipBlockAtLevel " ++ show n ++ ": []") []
    skipBlockAtLevel depth n ('/':'*':xs) = trace (replicate depth ' ' ++ "skipBlockAtLevel " ++ show n ++ ": /*") $ skipBlockAtLevel (depth + 2) (n+1) xs
    skipBlockAtLevel depth n ('*':'/':xs) = trace (replicate depth ' ' ++ "skipBlockAtLevel " ++ show n ++ ": */") $ skipBlockAtLevel (depth + 2) (n-1) xs
    skipBlockAtLevel depth n ('\n':xs) = trace (replicate depth ' ' ++ "skipBlockAtLevel " ++ show n ++ ": \\n") $ '\n' : skipBlockAtLevel depth n xs
    skipBlockAtLevel depth n (_:xs) = skipBlockAtLevel depth n xs

    goInString depth [] = trace (replicate depth ' ' ++ "goInString: []") []
    goInString depth ('\n':xs) = trace (replicate depth ' ' ++ "goInString: \\n") $ '\n' : goNormal (depth - 2) xs
    goInString depth ('\\':x:xs) = trace (replicate depth ' ' ++ "goInString: \\\" ++ show x) $ '\\' : x : goInString depth xs
    goInString depth ('"':xs) = trace (replicate depth ' ' ++ "goInString: \"" ) $ '"' : goNormal (depth - 2) xs
    goInString depth (c:cs) = goInString depth (c:cs)

    goInChar depth [] = trace (replicate depth ' ' ++ "goInChar: []") []
    goInChar depth ('\\':x:xs) = trace (replicate depth ' ' ++ "goInChar: \\\" ++ show x) $ '\\' : x : goInChar depth xs
    goInChar depth ('\'':xs) = trace (replicate depth ' ' ++ "goInChar: '") $ '\'' : goNormal (depth - 2) xs
    goInChar depth (c:cs) = goInChar depth (c:cs)

    trace :: String -> a -> a
    trace msg x = seq (unsafePerformIO (putStrLn msg)) x

import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
    let test1 = "/* outer /* inner */a"
    putStrLn $ "Test 1: " ++ show test1
    putStrLn $ "Trace:"
    putStrLn $ "Result: " ++ show (removeCommentsTrace test1)