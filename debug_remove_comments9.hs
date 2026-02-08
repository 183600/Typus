import Utils
import Data.List (isInfixOf, isPrefixOf, all, isSuffixOf)
import Data.Char (isSpace)

main :: IO ()
main = do
    let s = "\"a /*  */\""
    
    putStrLn $ "Testing string: " ++ show s
    putStrLn $ "isCompleteStringLiteral s: " ++ show (isCompleteStringLiteral s)
    putStrLn $ "blockComment in s: " ++ show ("/*" `isInfixOf` s)
    putStrLn $ "blockCommentEnd in s: " ++ show ("*/" `isInfixOf` s)
    
    let condition2 = isCompleteStringLiteral s && "/*" `isInfixOf` s && "*/" `isInfixOf` s
    putStrLn $ "condition2 (complete string literal with comment): " ++ show condition2
    
    if condition2
        then putStrLn "Would take branch: complete string literal with comment (our fix is here)"
        else putStrLn "Would NOT take branch: complete string literal with comment"