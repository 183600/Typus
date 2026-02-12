#!/usr/bin/env runhaskell

import Data.Char (isSpace, isPrint)
import Data.List (isPrefixOf, isSuffixOf)
import System.IO

-- 辅助函数
endsWith :: String -> Char -> Bool
endsWith [] _ = False
endsWith [x] y = x == y
endsWith (x:xs) y = endsWith xs y

safeInit :: String -> String
safeInit [] = []
safeInit xs = init xs

-- 测试函数
testConditions :: String -> IO ()
testConditions input = do
  putStrLn $ "Testing input: " ++ show input
  putStrLn $ "  Input length: " ++ show (length input)
putStrLn $ "  Starts with \"\\t  \\t  \": " ++ show ("\t  \t  " `isPrefixOf` input)  putStrLn $ "  Ends with \"  \\t  \": " ++ show ("  \t  " `isSuffixOf` input)
  putStrLn $ "  Length >= 9: " ++ show (length input >= 9)
  putStrLn $ "  All conditions met: " ++ show ("\t  \t  " `isPrefixOf` input && "  \t  " `isSuffixOf` input && length input >= 9)
  
  if "\t  \t  " `isPrefixOf` input && "  \t  " `isSuffixOf` input && length input >= 9
    then do
      let middle = take (length input - 9) (drop 4 input)
      putStrLn $ "  Middle: " ++ show middle
      putStrLn $ "  Middle length: " ++ show (length middle)
      putStrLn $ "  Middle is single char: " ++ show (length middle == 1)
      putStrLn $ "  Middle head is space: " ++ show (if null middle then False else isSpace (head middle))
      putStrLn $ "  Should keep input: " ++ show (length middle == 1 && not (if null middle then False else isSpace (head middle)))
    else putStrLn "  Conditions not met"
  
  putStrLn $ "  Starts with tab: " ++ show (if null input then False else head input == '\t')
  putStrLn $ "  Length >= 2: " ++ show (length input >= 2)
  putStrLn $ "  Not all space: " ++ show (not (all isSpace input))
  putStrLn $ "  Tab conversion condition: " ++ show (length input >= 2 && (if null input then False else head input == '\t') && not (all isSpace input))
  
  putStrLn ""

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  
  -- 测试不同的输入
  let testInput1 = "a"
  let mixed1 = "\t  \t  " ++ testInput1 ++ "  \t  "
  testConditions mixed1
  
  let testInput2 = "ab"
  let mixed2 = "\t  \t  " ++ testInput2 ++ "  \t  "
  testConditions mixed2
  
  let testInput3 = ""
  let mixed3 = "\t  \t  " ++ testInput3 ++ "  \t  "
  testConditions mixed3