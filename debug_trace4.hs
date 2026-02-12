import Utils
import Data.Char (isSpace, isControl)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  let input = "\t"
      inputLines = lines input
      singleLine = length inputLines <= 1
      allSpace = all isSpace input
  putStrLn $ "input = " ++ show input
  putStrLn $ "inputLines = " ++ show inputLines
  putStrLn $ "length inputLines <= 1 = " ++ show singleLine
  putStrLn $ "all isSpace input = " ++ show allSpace
  putStrLn $ "isPrefixOf \"\\t\\t\" input = " ++ show ("\t\t" `isPrefixOf` input)
  putStrLn $ "'\\t' `elem` input = " ++ show ('\t' `elem` input)
  putStrLn $ "' ' `elem` input = " ++ show (' ' `elem` input)
  putStrLn $ "not (all isSpace input) = " ++ show (not (all isSpace input))
  
  let result = normalizeIndentation input
  putStrLn $ "result = " ++ show result