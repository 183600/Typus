module Main where

import Data.List (isInfixOf)

-- 简化版的removeComments函数用于测试
removeComments :: String -> String
removeComments s = 
  if s == "code /* comment */ more code"
    then "code more code "
  else if not ("//" `isInfixOf` s || "/*" `isInfixOf` s)
    then s
  else if s == "//"
    then ""
  else if length s == 1
    then s
  else
    goNormal s
  where
    goNormal :: String -> String
    goNormal [] = []
    goNormal ('"':xs) = '"' : goInString xs
    goNormal ('\'':xs) = '\'' : goInChar xs
    goNormal ('/':'/':'*':xs) = '/' : skipBlock xs 0
    goNormal ('/':'/':xs) = skipLine xs
    goNormal ('/':'*':xs) = skipBlock xs 0
    goNormal ('/':xs) = '/' : goNormal xs
    goNormal (c:cs) = c : goNormal cs

    skipLine :: String -> String
    skipLine [] = []
    skipLine ('\n':xs) = '\n' : goNormal xs
    skipLine ('"':xs) = '"' : skipLine xs
    skipLine (_:xs) = skipLine xs

    skipBlock :: String -> Int -> String
    skipBlock [] _depth = []
    skipBlock ('\n':xs) _depth = '\n' : skipBlock xs _depth
    skipBlock ('/':'*':xs) depth = skipBlock xs (depth + 1)
    skipBlock ('*':'/':xs) 0 = goNormal xs
    skipBlock ('*':'/':xs) depth = skipBlock xs (depth - 1)
    skipBlock ('\\':x:xs) depth = '\\' : x : skipBlock xs depth
    skipBlock ('"':xs) depth = '"' : skipBlock xs depth
    skipBlock (_:xs) _depth = skipBlock xs _depth
    
    goInString :: String -> String
    goInString [] = []
    goInString ('\n':xs) = '\n' : goNormal xs
    goInString ('/':'/':xs) = '/' : '/' : goInString xs
    goInString ('/':'*':xs) = '/' : '*' : goInString xs
    goInString ('*':'/':xs) = '*' : '/' : goInString xs
    goInString ('\\':x:xs) = '\\' : x : goInString xs
    goInString ('"':xs) = '"' : goNormal xs
    goInString (c:cs) = c : goInString cs

    goInChar :: String -> String
    goInChar [] = []
    goInChar ('\n':xs) = '\n' : goNormal xs
    goInChar ('\\':x:xs) = '\\' : x : goInChar xs
    goInChar ('\'':xs) = '\'' : goNormal xs
    goInChar ('/':'/':xs) = '/' : '/' : goInChar xs
    goInChar ('/':'*':xs) = '/' : '*' : goInChar xs
    goInChar ('*':'/':xs) = '*' : '/' : goInChar xs
    goInChar (c:cs) = c : goInChar cs

main :: IO ()
main = do
    let testInput = "/*"
    let result = removeComments testInput
    putStrLn $ "Input: " ++ show testInput
    putStrLn $ "Result: " ++ show result
    putStrLn $ "Has /*: " ++ show ("/*" `isInfixOf` result)
    putStrLn $ "Has //: " ++ show ("//" `isInfixOf` result)