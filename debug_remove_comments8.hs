import Utils
import Data.List (isInfixOf, isPrefixOf, all, isSuffixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
    let s = "\"a /*  */\""
    
    putStrLn $ "Testing string: " ++ show s
    
    putStrLn $ "null s: " ++ show (null s)
    putStrLn $ "all isSpace s: " ++ show (all isSpace s)
    
    let slashSlash = "//"
    putStrLn $ "slashSlash `isPrefixOf` s: " ++ show (slashSlash `isPrefixOf` s)
    
    putStrLn $ "blockCommentPrefix && blockCommentSuffix: " ++ show ("/*" `isPrefixOf` s && "*/" `isSuffixOf` s)
    putStrLn $ "s == quote: " ++ show (s == "\"")
    putStrLn $ "s == singleQuote: " ++ show (s == "'")
    putStrLn $ "length s == 1 && s == backslash: " ++ show (length s == 1 && s == "\\")
    putStrLn $ "length s == 1: " ++ show (length s == 1)
    
    let specialPattern = "\"\\/\*"
    putStrLn $ "specialPattern `isPrefixOf` s: " ++ show (specialPattern `isPrefixOf` s)
    
    let condition1 = (not (null s) && case s of (c:_) -> c == '"' || c == '\''; [] -> False) && 
                     not (isCompleteStringLiteral s) && "/*" `isInfixOf` s
    putStrLn $ "condition1 (not complete string literal with comment): " ++ show condition1
    
    let pattern = "//* /"
    putStrLn $ "pattern `isInfixOf` s: " ++ show (pattern `isInfixOf` s)
    
    let condition2 = isCompleteStringLiteral s && "/*" `isInfixOf` s && "*/" `isInfixOf` s
    putStrLn $ "condition2 (complete string literal with comment): " ++ show condition2
    
    -- Check which branch would be taken
    if null s
        then putStrLn "Would take branch: null s"
    else if all isSpace s
        then putStrLn "Would take branch: all isSpace s"
    else if slashSlash `isPrefixOf` s
        then putStrLn "Would take branch: slashSlash prefix"
    else if "/*" `isPrefixOf` s && "*/" `isSuffixOf` s
        then putStrLn "Would take branch: blockCommentPrefix and blockCommentSuffix"
    else if s == "\""
        then putStrLn "Would take branch: s == quote"
    else if s == "'"
        then putStrLn "Would take branch: s == singleQuote"
    else if length s == 1 && s == "\\"
        then putStrLn "Would take branch: length s == 1 && s == backslash"
    else if length s == 1
        then putStrLn "Would take branch: length s == 1"
    else if specialPattern `isPrefixOf` s
        then putStrLn "Would take branch: specialPattern prefix"
    else if condition1
        then putStrLn "Would take branch: not complete string literal with comment"
    else if pattern `isInfixOf` s
        then putStrLn "Would take branch: pattern"
    else if condition2
        then putStrLn "Would take branch: complete string literal with comment (our fix is here)"
    else
        putStrLn "Would take branch: goNormal"