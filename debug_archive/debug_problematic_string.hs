import Utils hiding (isProblematicUnclosedString, isCompleteStringLiteral)
import Data.List

main :: IO ()
main = do
    let input = "\"// not comment\""
    let xs = tail input  -- 去掉第一个引号
    putStrLn $ "Input: " ++ show input
    putStrLn $ "Tail: " ++ show xs
    putStrLn $ "First 10 chars: " ++ show (take 10 xs)
    putStrLn $ "Is problematic: " ++ show (isProblematicUnclosedString ('"':take 10 xs))
    
    putStrLn "\nChecking isCompleteStringLiteral:"
    putStrLn $ "isCompleteStringLiteral result: " ++ show (isCompleteStringLiteral ('"':take 10 xs))

isProblematicUnclosedString :: String -> Bool
isProblematicUnclosedString s = 
  if null s 
    then False
    else case s of
      "\\" -> True
      "'" -> True
      "\"\\" -> True
      "\"\\\"" -> True
      "'\\" -> True
      (c:_) -> c `elem` ['\"', '\''] && not (isCompleteStringLiteral s)

isCompleteStringLiteral :: String -> Bool
isCompleteStringLiteral str = 
  case str of
    [] -> False
    ['\''] -> False
    ['\"'] -> False
    "\"\\" -> True
    "\\" -> False
    "\"" -> False
    "'" -> False
    (c:rest) -> case c of
           '"' -> hasClosingQuote '"' rest
           '\'' -> False  -- 单引号字符串总是返回False

hasClosingQuote :: Char -> String -> Bool
hasClosingQuote quote [] = False
hasClosingQuote quote (x:xs) = 
  if x == quote then True  -- 找到闭合引号，是完整的字符串
  else hasClosingQuote quote xs  -- 其他字符，继续查找