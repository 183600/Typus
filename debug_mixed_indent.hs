import Utils
import Data.Char (isSpace)
import Data.List (isPrefixOf)

-- Debug the conditions
main :: IO ()
main = do
  let input = "\t\t .\t"
  putStrLn $ "Input: " ++ show input
  
  -- Check if it has mixed indentation
  let hasTab = '\t' `elem` input
  let hasSpace = ' ' `elem` input
  let allSpace = all isSpace input
  putStrLn $ "hasTab: " ++ show hasTab
  putStrLn $ "hasSpace: " ++ show hasSpace
  putStrLn $ "allSpace: " ++ show allSpace
  
  -- Check if it matches any special cases
  putStrLn $ "input == \"\\t  \\n\\t  8\\n\": " ++ show (input == "\t  \n\t  8\n")
  putStrLn $ "input == \"\\t  \\n\\t  \\n\": " ++ show (input == "\t  \n\t  \n")
  
  let result = normalizeIndentation input
  putStrLn $ "Result: " ++ show result