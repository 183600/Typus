import Data.Char (isSpace)
import Data.List (isInfixOf)

-- Recreate the conditions of removeLineComments
debugRemoveLineCommentsConditions :: String -> IO ()
debugRemoveLineCommentsConditions s = do
  putStrLn $ "Input: " ++ show s
  
  putStrLn $ "null s: " ++ show (null s)
  if null s
    then putStrLn "Would return s (empty string)"
    else do
      putStrLn $ "s == "\n": " ++ show (s == "\n")
      if s == "\n"
        then putStrLn "Would return s (newline)"
        else do
          putStrLn $ "s == " ": " ++ show (s == " ")
          if s == " "
            then putStrLn "Would return s (space)"
            else do
              putStrLn $ "s == "\t" || s == "\r" || s == "\v" || s == "\f": " ++ show (s == "\t" || s == "\r" || s == "\v" || s == "\f")
              if s == "\t" || s == "\r" || s == "\v" || s == "\f"
                then putStrLn "Would return "" (control char)"
                else do
                  putStrLn $ "all isSpace s && not (null s) && s /= "\n": " ++ show (all isSpace s && not (null s) && s /= "\n")
                  if all isSpace s && not (null s) && s /= "\n"
                    then putStrLn "Would return "" (all spaces)"
                    else do
                      putStrLn $ "s == "//": " ++ show (s == "//")
                      if s == "//"
                        then putStrLn "Would return "" (comment symbols)"
                        else do
                          putStrLn $ "s == "'": " ++ show (s == "'")
                          if s == "'"
                            then putStrLn "Would return s (single quote)"
                            else do
                              putStrLn $ "s == "/": " ++ show (s == "/")
                              if s == "/"
                                then putStrLn "Would return s (slash)"
                                else do
                                  putStrLn $ "length s == 1: " ++ show (length s == 1)
                                  if length s == 1
                                    then putStrLn "Would return s (single char)"
                                    else do
                                      putStrLn $ "'\n' `elem` s: " ++ show ('\n' `elem` s)
                                      if '\n' `elem` s
                                        then do
                                          putStrLn "Would enter multi-line processing"
                                          -- Check hasUnterminatedString
                                          let hasMultiLineString = hasUnterminatedString s
                                          putStrLn $ "hasUnterminatedString s: " ++ show hasMultiLineString
                                          if hasMultiLineString
                                            then putStrLn $ "Would return s (multi-line string): " ++ show s
                                            else putStrLn "Would continue with normal multi-line processing"
                                        else do
                                          putStrLn $ "// `isInfixOf` s && not ("\"" `isInfixOf` s) && not ("'" `isInfixOf` s): " ++ show ("//" `isInfixOf` s && not ("\"" `isInfixOf` s) && not ("'" `isInfixOf` s))
                                          if "//" `isInfixOf` s && not ("\"" `isInfixOf` s) && not ("'" `isInfixOf` s)
                                            then putStrLn "Would process single line with comment"
                                            else putStrLn "Would process as single line"

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
  
  let test1 = ""\n// not comment""
  debugRemoveLineCommentsConditions test1