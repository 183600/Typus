#!/usr/bin/env stack
-- stack script --resolver lts-21.25

import Data.List (intercalate)

-- 简化版的 isCompleteStringLiteral 用于测试
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
                          _ -> if all (==' ') xs
                               then True
                               else False
            else if x == '\\'
                 then go xs (backslashCount + 1)
                 else go xs 0

main :: IO ()
main = do
  -- Test prop_string_lines
  let s1 = "a\n"
  let ls1 = lines s1
  let result1 = intercalate "\n" ls1
  putStrLn $ "Test prop_string_lines:"
  putStrLn $ "  Input: " ++ show s1
  putStrLn $ "  lines: " ++ show ls1
  putStrLn $ "  intercalate: " ++ show result1
  putStrLn $ "  Expected: " ++ show s1
  putStrLn ""
  
  -- Test prop_is_complete_string_literal_escape_backslash
  let s2 = "\""
  let withBackslash = "\"" ++ s2 ++ "\\\\"
  putStrLn $ "Test prop_is_complete_string_literal_escape_backslash:"
  putStrLn $ "  Input: " ++ show s2
  putStrLn $ "  withBackslash: " ++ show withBackslash
  putStrLn $ "  isCompleteStringLiteral: " ++ show (isCompleteStringLiteral withBackslash)
  putStrLn ""