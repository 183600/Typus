import Utils (removeLineComments)
import Data.List (isInfixOf, intercalate)

main :: IO ()
main = do
  let input = "code\n// comment1\n// comment2\nmore code"
      result = removeLineComments input
      inputLines = lines input
      processedLines = map (\line -> if "//" `isInfixOf` line then "" else line) inputLines
      unlinesResult = unlines processedLines
      intercalateResult = intercalate "\n" processedLines
      hasTrailingNewline = not (null input) && last input == '\n'
  putStrLn $ "input: " ++ show input
  putStrLn $ "inputLines: " ++ show inputLines
  putStrLn $ "processedLines: " ++ show processedLines
  putStrLn $ "hasTrailingNewline: " ++ show hasTrailingNewline
  putStrLn $ "unlinesResult: " ++ show unlinesResult
  putStrLn $ "intercalateResult: " ++ show intercalateResult
  putStrLn $ "result: " ++ show result