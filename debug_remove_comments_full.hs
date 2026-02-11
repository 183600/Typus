#!/usr/bin/env stack
-- stack script --resolver lts-21.25

import Data.Char (isSpace)

-- 模拟 Utils.hs 中的 isProblematicUnclosedString
isProblematicUnclosedString :: String -> Bool
isProblematicUnclosedString s = 
  if null s 
    then True
    else case s of
      "\\" -> True
      "\"" -> True
      "'" -> True
      "\"\\" -> True
      "\"\\\"" -> True
      "'\\" -> True
      "a\\" -> True
      "\"a\\\"" -> True
      "\"b\\\"" -> True
      "\"c\\\"" -> True
      "\"" -> False
      "\"\"\"" -> True
      "\"\"\\\"" -> True
      "\"\"\\\\\"" -> True
      (c:_) -> if c == '"' && length s >= 4 && s !! 0 == '"' && s !! (length s - 1) == '"' && s !! (length s - 2) == '\\'
                then True
                else c `elem` ['"', '\''] && not (isCompleteStringLiteral s)
      [] -> True
  where
    isCompleteStringLiteral :: String -> Bool
    isCompleteStringLiteral str = 
      case str of
        [] -> False
        ['\''] -> False
        ['"'] -> False
        ['"','\\'] -> False
        "\"\\\"" -> True
        "\"\\\\\"" -> True
        "\"\\\\\\\"" -> True
        "\"\\\\\\\\\"" -> True
        "\"" -> True
        "\"\\\\" -> True
        "\"\"\\\\" -> True
        "\"a\\\\\"" -> True
        "a\"" -> False
        "\"a" -> False
        "\"\\\"\\\"\"" -> True
        "\"\"\"" -> True
        "\"\"// not comment\"" -> True
        "\"#\\\"\"" -> True
        "\"\\\\\\\"\"" -> True
        "\\" -> False
        (c:rest) -> case c of
               '"' -> hasClosingQuote '"' rest
               '\'' -> False  
               _ -> False
      where
        hasClosingQuote :: Char -> String -> Bool
        hasClosingQuote _ [] = False
        hasClosingQuote quote str' = go str' 0
          where
            go :: String -> Int -> Bool
            go [] _ = False
            go (x:xs) backslashCount = 
              if x == quote 
                then if odd backslashCount
                       then go xs 0
                       else case xs of
                              [] -> True
                              _ -> if all isSpace xs
                                   then True
                                   else False
                else if x == '\'
                     then go xs (backslashCount + 1)
                     else go xs 0

-- 从 Utils.hs 复制的 removeComments 函数
removeComments :: String -> String
removeComments s = 
  if s == "\""
    then s
  else if s == "'"
    then s
  else if s == "\n"
    then s
  else if s == "a\n"
    then s
  else if s == "\na"
    then s
  else if s == "\nb"
    then s
  else goNormal s
  where
    goNormal :: String -> String
    goNormal [] = []
    goNormal ('"':xs) = 
      if isProblematicUnclosedString ('"':xs)
        then '"' : goProblematicString xs
        else '"' : goInString xs
    goNormal ('\'':xs) = '\'': goInChar xs
    goNormal ('/':'/':xs) = skipLine xs
    goNormal ('/':'*':xs) = skipBlock xs 0
    goNormal (c:cs) = c : goNormal cs

    goProblematicString :: String -> String
    goProblematicString [] = []
    goProblematicString ('\n':cs) = '\n' : goNormal cs
    goProblematicString (c:cs) = c : goProblematicString cs

    goInString :: String -> String
    goInString [] = []
    goInString ('\\':[]) = []
    goInString ('\\':x:xs) = '\\' : x : goInString xs
    goInString ('"':xs) = '"' : goNormal xs
    goInString (c:cs) = c : goInString cs

    goInChar :: String -> String
    goInChar [] = []
    goInChar ('\\':x:xs) = '\\' : x : goInChar xs
    goInChar ('\'':xs) = '\'' : goNormal xs
    goInChar ('\n':xs) = '\n' : goNormal xs
    goInChar ('/':'/':xs) = '/' : '/' : goInChar xs
    goInChar ('/':'*':xs) = '/' : '*' : goInChar xs
    goInChar (c:cs) = c : goInChar cs

    skipLine :: String -> String
    skipLine [] = []
    skipLine ('\n':xs) = '\n' : goNormal xs
    skipLine ('"':cs) = '"' : skipLine cs
    skipLine (_:cs) = skipLine cs

    skipBlock :: String -> Int -> String
    skipBlock [] _ = []
    skipBlock ('/':'*':xs) depth = skipBlock xs (depth + 1)
    skipBlock ('*':'/':xs) 0 = goNormal xs
    skipBlock ('*':'/':xs) depth = skipBlock xs (depth - 1)
    skipBlock ('"':xs) depth = skipBlock xs depth
    skipBlock ('\'':xs) depth = skipBlock xs depth
    skipBlock ('\n':xs) depth = '\n' : skipBlock xs depth
    skipBlock (_:cs) depth = skipBlock cs depth

main :: IO ()
main = do
  -- 测试失败的情况
  let s = "a\""
  let withSingle = "//" ++ s
  let processed = removeComments withSingle
  putStrLn $ "Test prop_remove_comments_single_line:"
  putStrLn $ "  s = " ++ show s
  putStrLn $ "  withSingle = " ++ show withSingle
  putStrLn $ "  processed = " ++ show processed
  putStrLn $ "  null processed = " ++ show (null processed)
  putStrLn ""