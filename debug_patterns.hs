#!/usr/bin/env stack
-- stack script --resolver lts-21.25

import Data.List (intercalate)

-- 从 Utils.hs 复制的相关模式
testPatterns :: [(String, Bool)]
testPatterns = 
  [ ("\"\\\"", True)
  , ("\"\\\\\"", True)
  , ("\"\\\\\\\"", True)
  , ("\"\\\\\\\\\"", True)
  , ("\"\"", True)
  , ("\"\\", True)
  , ("\"\"\\", True)
  , ("\"\\\"\\\"\"", True)
  , ("\"\\\\\\\"\"", True)
  , ("\\", False)
  ]

-- 模拟 isCompleteStringLiteral 的模式匹配
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
    "\"\"" -> True
    "\"\\\" -> True
    "\"\"\\" -> True
    "\"a\\\"" -> True
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
                          _ -> if all (==' ') xs
                               then True
                               else False
            else if x == '\\'
                 then go xs (backslashCount + 1)
                 else go xs 0

main :: IO ()
main = do
  putStrLn "Testing patterns:"
  mapM_ testPattern testPatterns
  putStrLn ""
  
  -- Test the specific failing case
  let s = "\""
  let withBackslash = "\"" ++ s ++ "\\\\"
  putStrLn $ "Failing test case:"
  putStrLn $ "  s = " ++ show s
  putStrLn $ "  withBackslash = " ++ show withBackslash
  putStrLn $ "  isCompleteStringLiteral withBackslash = " ++ show (isCompleteStringLiteral withBackslash)
  where
    testPattern (str, expected) = do
      let result = isCompleteStringLiteral str
      let status = if result == expected then "OK" else "FAIL"
      putStrLn $ "  " ++ show str ++ " -> " ++ show result ++ " (expected " ++ show expected ++ ") [" ++ status ++ "]"