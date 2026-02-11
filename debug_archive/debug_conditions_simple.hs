import Data.Char (isSpace)
import Data.List (isInfixOf)

-- | 检查是否包含未闭合的字符串字面量（跨行）
hasUnterminatedString :: String -> Bool
hasUnterminatedString s = go 0 False s
  where
    go :: Int -> Bool -> String -> Bool
    go _ _ [] = False
    go count inString ('\\':c:cs) = go count inString (c:cs)  -- Skip escaped characters
    go count True ('"':cs) = go (count + 1) False cs  -- Close string
    go count False ('"':cs) = go (count + 1) True cs   -- Open string
    go count inString ('\n':cs) = if inString then True else go count False cs  -- Check for newline in string
    go count inString (_:cs) = go count inString cs

main :: IO ()
main = do
  putStrLn "=== Debugging removeLineComments conditions ==="
  
  let test1 = "\"\n// not comment\""
  putStrLn $ "Input: " ++ show test1
  
  let newline = '\n'
  putStrLn $ "Has newline: " ++ show (newline `elem` test1)
  
  let hasMultiLineString = hasUnterminatedString test1
  putStrLn $ "Has unterminated string: " ++ show hasMultiLineString
  
  if hasMultiLineString
    then putStrLn $ "Should return original: " ++ show test1
    else putStrLn "Would continue with normal processing"