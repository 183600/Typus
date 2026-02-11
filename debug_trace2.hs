import Utils
import Data.Char (isSpace)

-- Test with debug trace
main :: IO ()
main = do
  let input = "\t  \n\t  \n"
  putStrLn $ "Input: " ++ show input
  
  -- Check what all isSpace returns
  putStrLn $ "all isSpace input: " ++ show (all isSpace input)
  
  -- Check what lines returns
  let inputLines = lines input
  putStrLn $ "lines input: " ++ show inputLines
  putStrLn $ "length inputLines: " ++ show (length inputLines)
  
  -- Check if it's all whitespace
  let allWhitespace = all isSpace input
  putStrLn $ "allWhitespace: " ++ show allWhitespace
  
  -- Check the condition that's being evaluated
  let cond = length inputLines <= 1
  putStrLn $ "length inputLines <= 1: " ++ show cond
  
  let result = normalizeIndentation input
  putStrLn $ "Output: " ++ show result