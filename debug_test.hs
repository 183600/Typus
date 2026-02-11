import Data.Char (isSpace)
import Data.List (isInfixOf)

main :: IO ()
main = do
    let s = " "
    let withComment = s ++ "// comment"
    putStrLn $ "Input: " ++ show s
    putStrLn $ "With comment: " ++ show withComment
    
    -- 测试不同的条件
    putStrLn $ "null s: " ++ show (null s)
    putStrLn $ "s == newline: " ++ show (s == "\n")
    putStrLn $ "all isSpace s && s /= newline: " ++ show (all isSpace s && s /= "\n")
    putStrLn $ "s == //: " ++ show (s == "//")
    putStrLn $ "s == ': " ++ show (s == "'")
    putStrLn $ "s == /: " ++ show (s == "/")
    putStrLn $ "length s == 1: " ++ show (length s == 1)
    
    -- 测试 withComment
    putStrLn "\n--- Testing withComment ---"
    let wc = withComment
    putStrLn $ "null wc: " ++ show (null wc)
    putStrLn $ "wc == newline: " ++ show (wc == "\n")
    putStrLn $ "all isSpace wc && wc /= newline: " ++ show (all isSpace wc && wc /= "\n")
    putStrLn $ "wc == //: " ++ show (wc == "//")
    putStrLn $ "wc == ': " ++ show (wc == "'")
    putStrLn $ "wc == /: " ++ show (wc == "/")
    putStrLn $ "length wc == 1: " ++ show (length wc == 1)
    putStrLn $ "// isInfixOf wc: " ++ show ("//" `isInfixOf` wc)
    putStrLn $ "not quote isInfixOf wc: " ++ show (not ("\"" `isInfixOf` wc))
    putStrLn $ "not singleQuote isInfixOf wc: " ++ show (not ("'" `isInfixOf` wc))
    putStrLn $ "not newline elem wc: " ++ show (not ('\n' `elem` wc))