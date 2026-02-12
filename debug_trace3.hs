import Utils
import Data.Char (isSpace, isControl)

main :: IO ()
main = do
  let input = "\t"
      condition1 = any (\c -> isControl c && c `notElem` ['\n', '\r', '\t', '\f', '\v']) input
      condition2 = input == " "
      condition3 = input == "\n"
      condition4 = input == "\n\n"
  putStrLn $ "input = " ++ show input
  putStrLn $ "condition1 (control char not in list): " ++ show condition1
  putStrLn $ "condition2 (space): " ++ show condition2
  putStrLn $ "condition3 (newline): " ++ show condition3
  putStrLn $ "condition4 (double newline): " ++ show condition4
  
  let result = normalizeIndentation input
  putStrLn $ "result = " ++ show result